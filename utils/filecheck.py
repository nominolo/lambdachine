#!/usr/bin/python
# A small python script used to check the validitiy of an output. This utility
# is a limited version of the FileCheck utility used in the llvm project:
#
#   http://llvm.org/cmds/FileCheck.html
#
# FileCheck does a line-by line check of a file that validates whether it
# contains the expected content.  This is useful for regression tests etc.
#
# This program exits with an error status of 2 on error, exit status of 0 if
# the file matched the expected contents, and exit status of 1 if it did not
# contain the expected contents.
#
# usage: filecheck.py <check_file>
#
# The input is read from stdin and checked against the patterns in <check_file>
# The <check_file> is read, looking for lines that start with "CHECK: ". The
# text after the CHECK marker is the pattern to match. By default the patterns
# are literal strings. Regex patterns are enabled by surrouding the regex
# portion in {{}} braces.
#
import sys
import os
import os.path
import re
import collections

usage = """\
usage: filecheck.py <check_file>
  Checks the standard input against patterns in <check_file>"""

class Exit:
  Ok    = 0
  Fail  = 1 # Match failure
  Error = 2 # Other error

  @staticmethod
  def error(msg):
    print("ERROR: "+msg)
    sys.exit(Exit.Error)
  @staticmethod
  def fail(check):
    print("Failed to match pattern: "+str(check))
    sys.exit(Exit.Fail)
  @staticmethod
  def ok():
    sys.exit(Exit.Ok)

class CheckString:
  def __init__(self, pattern, lineno=-1):
    self.patstr  = pattern
    self.regex   = parse_pattern(pattern, lineno)
    self.loc     = lineno

  def match(self, line):
    return re.search(self.regex, line)

  def __str__(self):
    return self.patstr
  
  def __repr__(self):
    return "CheckString(\""+self.patstr+"\", lineno="+str(self.loc)+")"
  
def parse_pattern(pat, lineno=-1):
  """ Parse a check pattern string
      
      Check strings are mostly fixed strings. Regular expressions are allowed
      to be mixed with the strings. A regex is surrounded by a pair of {{ }}
      braces.
  """
  if len(pat) == 0:
    Exit.error("Empty pattern string on line "+str(lineno))
  
  # Fast case for fixed string without regex
  if "{{" not in pat:
    return re.escape(pat)

  # There is at least one regex piece.  Build up the regex pattern
  # by escaping scary characters in fixed strings, building up one big regex.
  res = ""
  while len(pat) > 0:
    start = pat.find("{{")
    if start == -1:
      res += re.escape(pat)
      break
    else:
      end = pat.find("}}")
      if end == -1:
        Exit.error("Missing closing }} in regex pattern: "+pat)
      # Grab fixed part
      res += re.escape(pat[:start])
      # Enclose {{}} patterns in parens even though we're not
      # capturing the result for any purpose.  This is required in case the
      # expression contains an alternation like: CHECK:  abc{{x|z}}def.  We
      # want this to turn into: "abc(x|z)def" not "abcx|zdef".
      res += "(" + pat[start+2:end] + ")"
      pat  = pat[end+2:]

  return res

def read_check_file(fname, prefix="CHECK: "):
  """ Reads patterns from a file used to validate an output file

      read_check_file :: String -> [CheckString]

      Patterns are found by looking for lines that contains CHECK: 
      The value following the CHECK: marker is used as a pattern to validate
      the input.
  """
  if not os.path.exists(fname):
    Exit.error("File does not exist: "+fname)
  
  checks = collections.deque()
  for (lineno, line) in enumerate(file(fname)):
    pos = line.find(prefix)
    if pos != -1:
      pat = line[pos+len(prefix):].rstrip()
      checks.append(CheckString(pat, lineno+1))
  return checks

def main():
  if len(sys.argv) != 2:
    print(usage)
    Exit.error("No check file given as input")
  
  checks = read_check_file(sys.argv[1])
  if len(checks) == 0:
    Exit.error("No checks found in input file")

  check  = checks.popleft()
  for line in sys.stdin:
    match = check.match(line)
    while match:
      if len(checks) == 0: Exit.ok()
      pos   = match.end()
      check = checks.popleft()
      match = check.match(line[pos:])

  # If we have read all lines then we still have an active check so fail 
  Exit.fail(check)

if __name__ == "__main__":
  main()


