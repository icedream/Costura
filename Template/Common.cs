using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.IO.Compression;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security.AccessControl;
using System.Security.Cryptography;
using System.Security.Principal;
using System.Text;
using System.Threading;

static class Common
{
    private const int DelayUntilReboot = 4;

    [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
    static extern bool MoveFileEx(string lpExistingFileName, string lpNewFileName, int dwFlags);

    [DllImport("kernel32", SetLastError = true, CharSet = CharSet.Unicode)]
    static extern IntPtr LoadLibrary(string dllToLoad);

    [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
    [return: MarshalAs(UnmanagedType.Bool)]
    static extern bool SetDllDirectory(string lpPathName);

    [DllImport("libdl.so")]
    static extern IntPtr dlopen(string filename, int flags);

    [DllImport("/usr/lib/libSystem.dylib", EntryPoint="dlopen")]
    static extern IntPtr dlopenMac(string filename, int flags);

    [Conditional("DEBUG")]
    public static void Log(string format, params object[] args)
    {
        // Should this be trace?
        Debug.WriteLine("=== COSTURA === " + string.Format(format, args));
    }

    static void CopyTo(Stream source, Stream destination)
    {
        var array = new byte[81920];
        int count;
        while ((count = source.Read(array, 0, array.Length)) != 0)
        {
            destination.Write(array, 0, count);
        }
    }

    static void CreateDirectory(string tempBasePath)
    {
        if (!Directory.Exists(tempBasePath))
        {
            Directory.CreateDirectory(tempBasePath);
        }
    }

    static byte[] ReadStream(Stream stream)
    {
        var data = new Byte[stream.Length];
        stream.Read(data, 0, data.Length);
        return data;
    }

    public static string CalculateChecksum(string filename)
    {
        using (FileStream fs = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))
        using (BufferedStream bs = new BufferedStream(fs))
        {
            using (SHA1Managed sha1 = new SHA1Managed())
            {
                byte[] hash = sha1.ComputeHash(bs);
                StringBuilder formatted = new StringBuilder(2 * hash.Length);
                foreach (byte b in hash)
                {
                    formatted.AppendFormat("{0:X2}", b);
                }
                return formatted.ToString();
            }
        }
    }

    public static Assembly ReadExistingAssembly(AssemblyName name)
    {
        var currentDomain = AppDomain.CurrentDomain;
        var assemblies = currentDomain.GetAssemblies();
        foreach (var assembly in assemblies)
        {
            var currentName = assembly.GetName();
            if (string.Equals(currentName.Name, name.Name, StringComparison.InvariantCultureIgnoreCase) &&
                string.Equals(CultureToString(currentName.CultureInfo), CultureToString(name.CultureInfo), StringComparison.InvariantCultureIgnoreCase))
            {
                Log("Assembly '{0}' already loaded, returning existing assembly", assembly.FullName);

                return assembly;
            }
        }
        return null;
    }

    static string CultureToString(CultureInfo culture)
    {
        if (culture == null)
            return "";

        return culture.Name;
    }

    public static Assembly ReadFromDiskCache(string tempBasePath, AssemblyName requestedAssemblyName)
    {
        var name = requestedAssemblyName.Name.ToLowerInvariant();

        if (requestedAssemblyName.CultureInfo != null && !String.IsNullOrEmpty(requestedAssemblyName.CultureInfo.Name))
            name = String.Format("{0}.{1}", requestedAssemblyName.CultureInfo.Name, name);

        var assemblyTempFilePath = Path.Combine(tempBasePath, String.Concat(name, ".dll"));
        if (File.Exists(assemblyTempFilePath))
        {
            return Assembly.LoadFile(assemblyTempFilePath);
        }
        assemblyTempFilePath = Path.ChangeExtension(assemblyTempFilePath, "exe");
        if (File.Exists(assemblyTempFilePath))
        {
            return Assembly.LoadFile(assemblyTempFilePath);
        }
        assemblyTempFilePath = Path.Combine(tempBasePath, String.Concat(name, ".dll"));
        if (File.Exists(assemblyTempFilePath))
        {
            return Assembly.LoadFile(assemblyTempFilePath);
        }
        assemblyTempFilePath = Path.ChangeExtension(assemblyTempFilePath, "exe");
        if (File.Exists(assemblyTempFilePath))
        {
            return Assembly.LoadFile(assemblyTempFilePath);
        }
        return null;
    }

    public static Assembly ReadFromEmbeddedResources(Dictionary<string, string> assemblyNames, Dictionary<string, string> symbolNames, AssemblyName requestedAssemblyName)
    {
        var name = requestedAssemblyName.Name.ToLowerInvariant();

        if (requestedAssemblyName.CultureInfo != null && !String.IsNullOrEmpty(requestedAssemblyName.CultureInfo.Name))
            name = String.Format("{0}.{1}", requestedAssemblyName.CultureInfo.Name, name);

        byte[] assemblyData;
        using (var assemblyStream = LoadStream(assemblyNames, name))
        {
            if (assemblyStream == null)
            {
                return null;
            }
            assemblyData = ReadStream(assemblyStream);
        }

        using (var pdbStream = LoadStream(symbolNames, name))
        {
            if (pdbStream != null)
            {
                var pdbData = ReadStream(pdbStream);
                return Assembly.Load(assemblyData, pdbData);
            }
        }

        return Assembly.Load(assemblyData);
    }

    static Stream LoadStream(Dictionary<string, string> resourceNames, string name)
    {
        string value;
        if (resourceNames.TryGetValue(name, out value))
            return LoadStream(value);

        return null;
    }

    static Stream LoadStream(string fullname)
    {
        var executingAssembly = Assembly.GetExecutingAssembly();
        Stream finalStream;

        if (fullname.EndsWith(".zip"))
        {
            using (var stream = executingAssembly.GetManifestResourceStream(fullname))
            {
                if (stream == null)
                    throw new NullReferenceException("Resource stream " + fullname + " is not available");
                using (var compressStream = new DeflateStream(stream, CompressionMode.Decompress))
                {
                    finalStream = new MemoryStream();
                    CopyTo(compressStream, finalStream);
                    finalStream.Position = 0;
                }
            }
        }
        else
        {
            finalStream = executingAssembly.GetManifestResourceStream(fullname);
        }

        if (finalStream == null)
            throw new NullReferenceException("Resource stream " + fullname + " could not be processed");

        return finalStream;
    }

    // Mutex code from http://stackoverflow.com/questions/229565/what-is-a-good-pattern-for-using-a-global-mutex-in-c
    public static void PreloadUnmanagedLibraries(string hash, string tempBasePath, IEnumerable<string> libs, Dictionary<string, string> checksums)
    {
        if (!IsWin32())
        {
            Thread.BeginCriticalRegion();
            ActualPreloadUnmanagedLibraries(tempBasePath, libs, checksums);
            Thread.EndCriticalRegion();
            return;
        }

        string mutexId = string.Format("Global\\Costura{0}", hash);

        using (var mutex = new Mutex(false, mutexId))
        {
            var allowEveryoneRule = new MutexAccessRule(new SecurityIdentifier(WellKnownSidType.WorldSid, null), MutexRights.FullControl, AccessControlType.Allow);
            var securitySettings = new MutexSecurity();
            securitySettings.AddAccessRule(allowEveryoneRule);
            mutex.SetAccessControl(securitySettings);

            var hasHandle = false;
            try
            {
                try
                {
                    hasHandle = mutex.WaitOne(60000, false);
                    if (hasHandle == false)
                        throw new TimeoutException("Timeout waiting for exclusive access");
                }
                catch (AbandonedMutexException)
                {
                    hasHandle = true;
                }

                ActualPreloadUnmanagedLibraries(tempBasePath, libs, checksums);
            }
            finally
            {
                if (hasHandle)
                    mutex.ReleaseMutex();
            }
        }
    }

    static void ActualPreloadUnmanagedLibraries(String tempBasePath, IEnumerable<string> libs, Dictionary<string, string> checksums)
    {
        CreateDirectory(Path.Combine(tempBasePath));
        InternalPreloadUnmanagedLibraries(tempBasePath, libs, checksums);
    }

    static void InternalPreloadUnmanagedLibraries(string tempBasePath, IEnumerable<string> libs, Dictionary<string, string> checksums)
    {
        string name;

        foreach (var lib in libs)
        {
            name = ResourceNameToPath(lib);

            var assemblyTempFilePath = Path.Combine(tempBasePath, name);

            if (File.Exists(assemblyTempFilePath))
            {
                var checksum = CalculateChecksum(assemblyTempFilePath);
                if (checksum != checksums[lib])
                    File.Delete(assemblyTempFilePath);
            }

            if (!File.Exists(assemblyTempFilePath))
            {
                using (var copyStream = LoadStream(lib))
                using (var assemblyTempFile = File.OpenWrite(assemblyTempFilePath))
                {
                    CopyTo(copyStream, assemblyTempFile);
                }
                DeleteFileOnReboot(assemblyTempFilePath);
            }
        }

        if (IsWin32())
        {
            SetDllDirectory(tempBasePath);
        }

        foreach (var lib in libs)
        {
            name = ResourceNameToPath(lib);

            if (name.EndsWith(".dll") && IsWin32())
            {
                var assemblyTempFilePath = Path.Combine(tempBasePath, name);

                LoadLibrary(assemblyTempFilePath);
            }
            else if (name.EndsWith(".dylib") && Environment.OSVersion.Platform == PlatformID.MacOSX)
            {
                var assemblyTempFilePath = Path.Combine(tempBasePath, name);
            }
            else if (name.EndsWith(".so") && Environment.OSVersion.Platform == PlatformID.Unix)
            {
                var assemblyTempFilePath = Path.Combine(tempBasePath, name);
            }
        }
    }

    private static void DeleteFileOnReboot(string path)
    {
        if (IsWin32())
        {
            if (!MoveFileEx(path, null, DelayUntilReboot))
            {
                //TODO: for now we ignore the return value.
            }
        }
        else
        {
            // Delete file when appdomain is unloaded
            AppDomain.CurrentDomain.DomainUnload += (sender, args) =>
            {
                File.Delete(path);
            };
        }
    }

    private static bool IsWin32()
    {
        return (Environment.OSVersion.Platform == PlatformID.Win32NT
            || Environment.OSVersion.Platform == PlatformID.Win32Windows
            || Environment.OSVersion.Platform == PlatformID.Win32S);
    }

    static string ResourceNameToPath(string lib)
    {
        var bittyness = IntPtr.Size == 8 ? "64" : "32";

        string name = lib;

        if (lib.StartsWith(String.Concat("costura", bittyness, ".")))
            name = lib.Substring(10);
        else if (lib.StartsWith("costura."))
            name = lib.Substring(8);

        if (name.EndsWith(".zip"))
            name = name.Substring(0, name.Length - 4);

        return name;
    }
}