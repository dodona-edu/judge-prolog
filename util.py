from threading import Thread
import codecs
import json
import os
import io


class SilentLimitedBuffer(io.StringIO):
    """Buffer that only read the first `maxsize` bytes of a stream of data.
    """

    def __init__(self, maxsize=5 * 1000 * 1000):
        io.StringIO.__init__(self, None)
        self.cursize = 0
        self.currealsize = 0
        self.maxsize = maxsize
        self.tooMuch = False
        self.done = False
        self.reader = None

    def write(self, s):
        size = len(s)
        self.currealsize += size
        if self.currealsize > self.maxsize:
            self.tooMuch = True
            return
        else:
            self.cursize += size
            return io.StringIO.write(self, s)

    def set_done(self):
        """Indicate that the reader should stop processing the input given via
        procces
        """

        self.done = True

    def getvalue(self):
        """Return the value that has been written the text
        "ERROR:    TRUNCATED X bytes ignored" is added

        Returns:
            string -- The written text as utf-8 string
        """

        if self.tooMuch:
            return (io.StringIO.getvalue(self) +
                    "\n\n" +
                    "ERROR:    TRUNCATED {} bytes ignored".format(self.currealsize - self.cursize))

        return io.StringIO.getvalue(self)

    def procces(self, stream):
        """Continiously read stream in thread until no more data comes trough
        or the procces in closed by calling set_done(). A deamon is used to
        ensure that swipl does not hang because the output is not read.

        We read per 100 bytes. We try to prevent unicode errors from cropping
        up during the execution. If our best effort fails, we ignore unknown
        unicode chars.

        Arguments:
            stream {file} -- something that is `.read` able

        Returns:
            SilentLimitedBuffer -- self, for chaining
        """

        def write_deamon(buff, stream):
            decoder = codecs.getincrementaldecoder("utf-8")(errors="ignore")

            while not buff.done:
                bytedata = stream.read(100)
                if not bytedata:
                    break  # no more data

                # The decoder keeps the tail of a chopped multibyte char
                data = decoder.decode(bytedata)
                if data:
                    buff.write(data)

        self.reader = Thread(target=write_deamon, args=(self, stream), daemon=True)
        self.reader.start()
        return self

    def wait_done(self, timeout=1):
        """Wait until the reader has seen the end of the stream.

        The wait is bounded because a process started by the student can keep
        the stream open after swipl is gone.
        """

        if self.reader is not None:
            self.reader.join(timeout=timeout)

    def retreive_and_stop(self):
        """get the value up to this point and and stop the reading procces.

        Returns:
            string -- The written text as utf-8 string
        """

        self.set_done()
        return self.getvalue()


def removeFile(filename):
    try:
        os.remove(filename)
    except OSError:
        pass


def readJson(filename):
    try:
        with open(filename, 'r') as f:
            return json.load(f)
    except (OSError, ValueError):
        return None
