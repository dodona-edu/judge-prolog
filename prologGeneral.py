import re
import subprocess

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


def removePath(s: str, testname):
    """Removes the path to the test file from the output

    Arguments:
        s {str} -- Text to clean

    Returns:
        str -- The cleaned text 
    """

    return re.sub(plMountdir, "", re.sub(plTestfile, "", s)).replace("plunit_" + testname + ":", "")


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
    a = subprocess.Popen(
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
        bufsize=bufsize,
        cwd=config["workdir"])

    didTimeout = None
    try:
        a.wait(timeout=timeout)
        didTimeout = False
    except subprocess.TimeoutExpired:
        a.terminate()
        didTimeout = True

    resStdOut = re.sub(plMountdir, "", a.stdout.read(
        bufsize).decode("utf-8")).splitlines(True)
    resStdErr = re.sub(plMountdir, "", a.stderr.read(
        bufsize).decode("utf-8")).splitlines(True)
    a.stderr.close()
    a.stdout.close()

    testcases += outputHandler(
        stdout=resStdOut,
        stderr=resStdErr,
        testname=testname,
        scriptfile=scriptfile,
        config=config,
        timeout=didTimeout
    )

    return testcases
