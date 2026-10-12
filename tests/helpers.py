import os
import sys

TESTS_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_DIR = os.path.dirname(TESTS_DIR)
EXERCISES_DIR = os.path.join(TESTS_DIR, "exercises")

sys.path.insert(0, REPO_DIR)
