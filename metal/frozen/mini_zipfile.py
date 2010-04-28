import struct

try:
    import zlib # We may need its compression method
except ImportError:
    zlib = None

class BadZipfile(Exception):
    pass

# constants for Zip file compression methods
ZIP_STORED = 0
ZIP_DEFLATED = 8
# Other ZIP compression methods not supported

# indexes of entries in the central directory structure
_CD_FILENAME_LENGTH = 12
_CD_EXTRA_FIELD_LENGTH = 13
_CD_COMMENT_LENGTH = 14
_CD_LOCAL_HEADER_OFFSET = 18

# indexes of entries in the local file header structure
_FH_COMPRESSION_METHOD = 4
_FH_COMPRESSED_SIZE = 8
_FH_FILENAME_LENGTH = 10
_FH_EXTRA_FIELD_LENGTH = 11


class ZipFile:

    def __init__(self, fp):
        """Open the ZIP file and read in the table of contents."""
        self.fileoffset = {}
        self.fp = fp

        # read last 22 bytes, end-of-directory record
        fp.seek(-22, 2)
        data = fp.read()
        if data[0:4] != "PK\005\006" or data[-2:] != "\000\000":
            raise BadZipfile, "File is not a zip file"
        endrec = struct.unpack("<4s4H2LH", data)
        fp.seek(endrec[6], 0)           # seek to start of central directory
        cd_end = endrec[6] + endrec[5]  # note end of central directory

        # read each central directory entry
        while fp.tell() < cd_end:
            centdir = fp.read(46)
            if centdir[0:4] != "PK\001\002":
                raise BadZipfile, "Bad magic number for central directory"
            centdir = struct.unpack("<4s4B4HlLL5HLL", centdir)
            filename = fp.read(centdir[_CD_FILENAME_LENGTH])
            self.fileoffset[filename] = centdir[_CD_LOCAL_HEADER_OFFSET]
            fp.seek(centdir[_CD_EXTRA_FIELD_LENGTH] +
                    centdir[_CD_COMMENT_LENGTH], 1)

    def exists(self, name):
        return name in self.fileoffset

    def read(self, name):
        # read the file header
        self.fp.seek(self.fileoffset[name], 0)
        fheader = self.fp.read(30)
        if fheader[0:4] != "PK\003\004":
            raise BadZipfile, "Bad magic number for file header"
        fheader = struct.unpack("<4s2B4HlLL2H", fheader)
        self.fp.seek(fheader[_FH_FILENAME_LENGTH] +
                fheader[_FH_EXTRA_FIELD_LENGTH], 1)
        compress_type = fheader[_FH_COMPRESSION_METHOD]

        # read file data and decompress if needed
        bytes = self.fp.read(fheader[_FH_COMPRESSED_SIZE])
        if compress_type == ZIP_STORED:
            return bytes
        elif compress_type == ZIP_DEFLATED and zlib:
            # zlib compress/decompress code by Jeremy Hylton of CNRI
            dc = zlib.decompressobj(-15)
            bytes = dc.decompress(bytes)
            # need to feed in unused pad byte so that zlib won't choke
            ex = dc.decompress('Z') + dc.flush()
            if ex:
                bytes = bytes + ex
            return bytes
        raise BadZipfile, \
              "Unsupported compression method %d for file %s" % \
              (compress_type, name)
