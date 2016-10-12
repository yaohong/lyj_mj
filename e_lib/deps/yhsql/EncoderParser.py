import os
import os.path
import codecs
import sys
import getopt
import stat
from optparse import OptionParser

global rootdir, filter, codes, file_list, file_restict
rootdir = "."
filter = [".c", ".cpp", ".h", ".hpp", ".cs", ".lua", ".erl", ".hrl", ".js", ".ts", ".py", ".bat", ".ps", ".sh", ".java", ".php", ".css", ".htm", ".html", ".xml", ".cfg", ".ini", ".shader", ".m"]
#filter = [".txt"]
codes = ["utf-8", "gb2312", "gbk", "utf-16"]
file_list = []
file_restict = False

def decode_utf8_bom(s):
    if s[:3] == codecs.BOM_UTF8:
        return s[3:]
    return s
    pass

def decode_utf16_bom(s):
    if s[:2] == codecs.BOM_UTF16:
        return s[2:]
    return s
    pass

def encode_utf8_bom(s):
    if s[:3] == codecs.BOM_UTF8:
        return s
    return codecs.BOM_UTF8 + s
    pass

def encode_utf16_bom(s):
    if s[:2] == codecs.BOM_UTF16:
        return s
    return codecs.BOM_UTF16 + s
    pass


def try_parse(bytes, codes):
    if len(codes) <= 0:
        print("parse error!!!")
        sys.exit()
        return None
        pass
    code = codes[0]
    try:
        s = bytes.decode(code)
        return s
        pass
    except:
        return try_parse(bytes, codes[1:])
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
    parser.add_option("-t", "--target", dest = "target", help = "target can be utf8bom/utf8nb/gb2312/gbk/utf16bom/utf16nb", action = "store", type = "string", default = "")
    parser.add_option("-d", "--dir", dest = "dir", help = "the root dir to parse", action = "store", type = "string", default = rootdir)
    parser.add_option("-f", "--filter", type = "string", help = "filter the exts", action = "callback", callback = filter_callback)
    parser.add_option("-n", "--filename", type = "string", help = "filename restiction", action = "callback", callback = filename_callback)
    
    (options, args) = parser.parse_args(args)

    def empty_fun(bytes):
        return bytes
        pass

    def parse_str(bytes):
        global codes
        bytes = decode_utf8_bom(bytes)
        bytes = decode_utf16_bom(bytes)
        return try_parse(bytes, codes)
        pass

    fun = empty_fun
    if options.target == "utf8bom" or options.target == "utf8":
        def utf8bom(bytes):
            s = parse_str(bytes)
            b = s.encode("utf-8")
            b = encode_utf8_bom(b)
            return b
            pass
        fun = utf8bom
    elif options.target == "utf8nb":
        def utf8nb(bytes):
            s = parse_str(bytes)
            b = s.encode("utf-8")
            b = decode_utf8_bom(b)
            return b
            pass
        fun = utf8nb
    elif options.target == "gb2312":
        def gb2312(bytes):
            s = parse_str(bytes)
            b = s.encode("gb2312")
            return b
            pass
        fun = gb2312
    elif options.target == "gbk":
        def gbk(bytes):
            s = parse_str(bytes)
            b = s.encode("gbk")
            return b
            pass
        fun = gbk
    elif options.target == "utf16bom" or options.target == "utf16":
        def utf16bom(bytes):
            s = parse_str(bytes)
            b = s.encode("utf-16")
            b = encode_utf16_bom(b)
            return b
            pass
        fun = utf16bom
    elif options.target == "utf16nb":
        def utf16nb(bytes):
            s = parse_str(bytes)
            b = s.encode("utf-16")
            b = decode_utf16_bom(b)
            return b
            pass
        fun = utf16nb
    else:
        print("action not valid:" + options.target)
        parser.print_help()
        #parser.error("action not valid:" + options.target)
        return
        pass
    
    f = ";".join(filter)
    #print("parsing rootdir:" + options.dir + ", target:" + options.target + ", filter:" + f)
    apply_all_files(options.dir, filter, fun)
    pass

cmd_args(sys.argv)
#raw_input()
