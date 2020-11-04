import re
import subprocess
from util import SilentLimitedBuffer

testfileName = '/tmp/tmp-testfile.pl'

plTestfile = re.compile(testfileName.replace(".", "\\.") + r"(:[0-9]*)?:?")
plStatus = re.compile(r"^[A.!+-]+$")
plResult = re.compile(r"^(ERROR|Warning): (.*)")
plDone = re.compile(r"done$")
plInfo = re.compile(r"^(ERROR: {5}|\t)(.*)")
plBeginTest = re.compile(r":- +begin_tests\(([^,]*)(,.*)?\)")
plEndTest = re.compile(r":- +end_tests\((.*)\)")
plComment = re.compile(r"%!(.*)")

plMountdir = re.compile(r"/mnt/[^/]*/")


def removePath(s: str, testname: str):
    """Removes the path to the test file from the output

    Arguments:
        s {str} -- Text to clean

    Returns:
        str -- The cleaned text
    """

    return (re.sub(plTestfile, "", removeMountDir(s))
            .replace("plunit_" + testname + ":", ""))


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
        a = {"accepted": False, "description": errorType, "messages": [
            {"format": "code", "description": d, "permission": "student"}]}
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


def swipl(scriptfile, testname, goal, outputHandler, timeout, config, bufsize=2500, removeMounts=True):
    testcases = []

    runner = subprocess.Popen(
        ['swipl',
         '-t', goal,
         '--quiet=yes',
         '--tty=yes',
         '--signals=no',
         "--stack_limit=" + config["prolog_stack_limit"],
         scriptfile,
         ],
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        bufsize=None,
        cwd=config["workdir"])

    # Keep reading bufffer while only saving the top bufsize bytes
    stdBuf = SilentLimitedBuffer(maxsize=bufsize).procces(runner.stderr)
    errBuf = SilentLimitedBuffer(maxsize=bufsize).procces(runner.stdout)

    didTimeout = True
    try:
        runner.wait(timeout=timeout)
        didTimeout = False
    except subprocess.TimeoutExpired:
        runner.terminate()

    resStdOut = stdBuf.retreive_and_stop()
    resStdErr = errBuf.retreive_and_stop()
    runner.stderr.close()
    runner.stdout.close()

    # Clean output and split in lines
    if removeMounts:
        resStdOut = removeMountDir(resStdOut).splitlines(True)
        resStdErr = removeMountDir(resStdErr).splitlines(True)
    else:
        resStdOut = resStdOut.splitlines(True)
        resStdErr = resStdErr.splitlines(True)
    # import sys
    # print("CAL", scriptfile, file=sys.stderr)
    # print("STD", "".join(resStdOut), file=sys.stderr)
    # print("ERR", "".join(resStdErr), file=sys.stderr)

    testcases += outputHandler(
        stdout=resStdOut,
        stderr=resStdErr,
        testname=testname,
        scriptfile=scriptfile,
        config=config,
        timeout=didTimeout
    )

    return testcases
