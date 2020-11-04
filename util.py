from threading import Thread
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
            danglingBytes = None  # will almost alway be None

            while not buff.done:
                bytedata = stream.read(100)
                if not bytedata:
                    break  # no more data

                # If some byte were not parsed in the previous run, prepend
                # them
                if danglingBytes:
                    bytedata = danglingBytes + bytedata
                    danglingBytes = None

                data = None
                # Errors might be thrown when a multibyte char is chopped
                try:
                    data = bytedata.decode("utf-8")
                    danglingBytes = None
                except UnicodeDecodeError:
                    for i in [-1, -2, -3]:
                        try:
                            data = bytedata[:i].decode("utf-8")
                            danglingBytes = bytedata[:i]
                            break
                        except UnicodeDecodeError:
                            pass
                    else:
                        # all failed (no break): ignore errors
                        data = bytedata.decode("utf-8", errors="ignore")
                        danglingBytes = None

                if data:
                    buff.write(data)

        Thread(target=write_deamon, args=(self, stream), daemon=True).start()
        return self

    def retreive_and_stop(self):
        """get the value up to this point and and stop the reading procces.

        Returns:
            string -- The written text as utf-8 string
        """

        self.set_done()
        return self.getvalue()


class CondFormatString:  # pylint: disable=too-few-public-methods
    """
    A class that can be used instead of string when using format

    a = CondFormatString(lambda x: x > 0,"{} was positive", "{} was negative")
    print(a.format(9)) # -> "9 was positive"
    print(a.format(-1)) # -> "-1 was negative"

    a = CondFormatString(
                lambda **d: d["failed"] > 0,
                "{failed} wrong of {numtests}",
                "All {numtests} tests correct")
    print(a.format(failed = 5, numtests = 9)) # -> 5 wrong of 9
    print(a.format(failed = 0, numtests = 9)) # -> All 9 tests correct
    """
    _fun = None

    def __init__(self, cond, sTrue, sFalse):
        self._cond = cond
        self._sTrue = sTrue
        self._sFalse = sFalse

    def format(self, *args, **kwargs):
        """
            Pass on calls to format to the condition evaluator and pick the right
            String to use
        """
        if self._cond(*args, **kwargs):
            return self._sTrue.format(*args, **kwargs)
        else:
            return self._sFalse.format(*args, **kwargs)


def removeFile(filename):
    try:
        os.remove(filename)
    except OSError:
        pass
