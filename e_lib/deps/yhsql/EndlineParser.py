import os
import os.path
import codecs
import sys
import getopt
import stat
from optparse import OptionParser

global rootdir, filter, target, file_list, file_restict
rootdir = "."
filter = [".c", ".cpp", ".h", ".hpp", ".cs", ".lua", ".erl", ".hrl", ".js", ".ts", ".py", ".bat", ".ps", ".sh", ".java", ".php", ".css", ".htm", ".html", ".xml", ".cfg", ".ini", ".shader", ".m"]
#filter = [".txt"]
target = ["win", "linux", "osx"]
file_list = []
file_restict = False

def all2linux(bytes):
    try:
        bytes = bytes.replace(b"\r\n", b"\n")
        bytes = bytes.replace(b"\r", b"\n")
        return bytes
        pass
    except:
        bytes = bytes.replace("\r\n", "\n")
        bytes = bytes.replace("\r", "\n")
        return bytes
        pass
    pass

def linux2osx(bytes):
    try:
        bytes = bytes.replace(b"\n", b"\r")
        return bytes
        pass
    except:
        bytes = bytes.replace("\n", "\r")
        return bytes
        pass
    pass

def linux2win(bytes):
    try:
        bytes = bytes.replace(b"\n", b"\r\n")
        return bytes
        pass
    except:
        bytes = bytes.replace("\n", "\r\n")
        return bytes
        pass
    pass

def apply_all_files(rootdir, filter, fun):
    global file_list, file_restict
    for parent, dirnames, filenames in os.walk(rootdir):
        for dirname in dirnames:
            pass
            #raw_input()

        for filename in filenames:
            file = os.path.join(parent, filename)
            name, ext = os.path.splitext(file)
            if (not file_restict and ext in filter) or (file_restict and filename in file_list):
                print("doing:" + file)
                os.chmod(file, stat.S_IWRITE)
                rf = open(file, "rb")
                bytes = rf.read()
                rf.close()
                bytes = fun(bytes)
                wf = open(file, "wb")
                wf.write(bytes)
                wf.close()
                pass
            #raw_input()
            pass
        pass
    pass

def cmd_args(args):
    def filter_callback(option, opt, value, parser):
        global filter
        filter = []
        tfilter = value.split(';')
        for f in tfilter:
            if len(f) > 0 and f[:1] != ".":
                f = "." + f
            filter.append(f)
            pass
        pass

    def filename_callback(option, opt, value, parser):
        global file_list, file_restict
        file_restict = True
        file_list = value.split(';')
        pass

    global rootdir, filter
    parser = OptionParser()
    parser.add_option("-t", "--target", dest = "target", help = "target can be windows(\\r\\n)/linux|android(\\n)/osx|ios(\\r)", action = "store", type = "string", default = "")
    parser.add_option("-d", "--dir", dest = "dir", help = "the root dir to parse", action = "store", type = "string", default = rootdir)
    parser.add_option("-f", "--filter", type = "string", help = "filter the exts", action = "callback", callback = filter_callback)
    parser.add_option("-n", "--filename", type = "string", help = "filename restiction", action = "callback", callback = filename_callback)
    
    (options, args) = parser.parse_args(args)

    def empty_fun(bytes):
        return bytes
        pass

    fun = empty_fun
    if options.target == "linux" or options.target == "android":
        fun = all2linux
    elif options.target == "win" or options.target == "windows" or options.target == "win32":
        def all2win(bytes):
            bytes = all2linux(bytes)
            bytes = linux2win(bytes)
            return bytes
            pass
        fun = all2win
    elif options.target == "osx" or options.target == "ios":
        def all2osx(bytes):
            bytes = all2linux(bytes)
            bytes = linux2osx(bytes)
            return bytes
            pass
        fun = all2osx
    else:
        print("action not valid:" + options.target)
        parser.print_help()
        #parser.error("action not valid:" + options.target)
        return
        pass
    
    f = ";".join(filter)
#    print("parsing rootdir:" + options.dir + ", target:" + options.target + ", filter:" + f)
    apply_all_files(options.dir, filter, fun)
    pass

cmd_args(sys.argv)
#raw_input()
