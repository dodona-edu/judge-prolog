import io
import re
import subprocess
import sys
import os
from threading import Thread

testfileName = '/tmp/tmp-testfile.pl'

plTestfile = re.compile(testfileName.replace(".", "\\.") + r"(:[0-9]*)?:?")
plMountdir = re.compile(r"/mnt/[^/]*/")
plStatus = re.compile(r"^[A.!+-]+$")
plResult = re.compile(r"^(ERROR|Warning): (.*)")
plDone = re.compile(r"done$")
plInfo = re.compile(r"^(ERROR:     |\t)(.*)")
plBeginTest = re.compile(r":- +begin_tests\(([^,]*)(,.*)?\)")
plEndTest = re.compile(r":- +end_tests\((.*)\)")
plComment = re.compile(r"%!(.*)")


def removePath(s: str, testname: str):
    """Removes the path to the test file from the output

    Arguments:
        s {str} -- Text to clean

    Returns:
        str -- The cleaned text 
    """

    return re.sub(plTestfile, "", removeMountDir(s)).replace("plunit_" + testname + ":", "")


def removeMountDir(s: str):
    return re.sub(plMountdir, "", s)


def analyse(errorType, data, errors):
    """Adds errors to the error array

    Arguments:
        errorType {str} -- The tye of the error
        data {list(str)} -- a list of lines of info
        errors {list(dict)} -- list to append to
    """
    if errorType is None:
        errorType = "compilation"

    if data:
        n = '\n'
        d = n.join(data)
        a = {
            "accepted": False,
            "description": errorType,
            "messages": [{"format": "code", "description": d, "permission": "student"}]
        }
        errors.append(a)


def checkErrors(lines, testname):
    errorType = None
    data = []
    testcases = []

    for line in lines:
        line = removePath(line.rstrip(), testname)
        isStatus = plStatus.match(line)
        if isStatus:
            analyse(errorType, data, testcases)
            data = []
        else:
            isResult = plResult.match(line)
            isInfo = plInfo.match(line)
            if isInfo:
                data.append(isInfo.group(2))
            elif isResult:
                analyse(errorType, data, testcases)
                moreinfo = isResult.group(2).strip()
                if moreinfo:
                    data = [moreinfo]
                else:
                    data = []
                errorType = isResult.group(1)
            else:
                data.append(line)
    analyse(errorType, data, testcases)
    return testcases


def swipl(scriptfile, testname, goal, outputHandler, timeout, config, bufsize=2500):
    testcases = []

    runner = subprocess.Popen(
        ['swipl',
         '-s', scriptfile,
         '-t', goal,
         '-q',
         '+tty',
         '--nosignals',
         "-L" + config["prolog_local_stack"],
         "-G" + config["prolog_global_stack"],
         "-T" + config["prolog_trail_stack"]
         ],
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        bufsize=None,
        cwd=config["workdir"])

    stdBuf = SilentLimitedBuffer(maxsize=bufsize).procces(runner.stderr)
    errBuf = SilentLimitedBuffer(maxsize=bufsize).procces(runner.stdout)

    didTimeout = None
    try:
        runner.wait(timeout=timeout)
        didTimeout = False
    except subprocess.TimeoutExpired:
        runner.terminate()
        didTimeout = True

    resStdOut = stdBuf.retreive_and_stop()
    resStdErr = errBuf.retreive_and_stop()
    runner.stderr.close()
    runner.stdout.close()

    # Clean output and split in lines
    resStdOut = removeMountDir(resStdOut).splitlines(True)
    resStdErr = removeMountDir(resStdErr).splitlines(True)

    #print("STD", "".join(resStdOut), file=sys.stderr)
    #print("ERR", "".join(resStdErr), file=sys.stderr)

    testcases += outputHandler(
        stdout=resStdOut,
        stderr=resStdErr,
        testname=testname,
        scriptfile=scriptfile,
        config=config,
        timeout=didTimeout
    )

    return testcases


def removeFile(filename):
    try:
        os.remove(filename)
    except OSError:
        pass


class SilentLimitedBuffer(io.StringIO):
    """Buffer that only read the first `maxsize` bytes of a 
    """

    def __init__(self, maxsize=5 * 1000 * 1000):
        io.StringIO.__init__(self, None)
        self.cursize = 0
        self.currealsize = 0
        self.maxsize = maxsize
        self.tooMuch = False
        self.done = False

    def write(self, s):
        l = len(s)
        self.currealsize += l
        if self.currealsize > self.maxsize:
            self.tooMuch = True
            return
        else:
            self.cursize += l
            return io.StringIO.write(self, s)

    def set_done(self):
        """Indicate that the reader should stop processing the input given via
        procces
        """

        self.done = True

    def getvalue(self):
        """Return the value that has been written the text "ERROR:    TRUNCATED 
        X bytes ignored" is added

        Returns:
            string -- The written text as utf-8 string
        """

        if self.tooMuch:
            return io.StringIO.getvalue(self) + "\n\nERROR:    TRUNCATED\n{} bytes ignored\n".format(self.currealsize - self.cursize)
        else:
            return io.StringIO.getvalue(self)

    def procces(self, stream):
        """Continiously read stream in thread until no more data comes trough 
        or the procces in closed by calling set_done() 

        Arguments:
            stream {file} -- something that is `.read` able

        Returns:
            SilentLimitedBuffer -- self, for chaining
        """

        def write_deamon(buff, stream):
            while(not buff.done):
                data = stream.read(100).decode("utf-8")
                if not data:
                    break  # no more data
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
