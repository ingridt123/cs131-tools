"""
Testing function comparing png files of trees
"""

import os

TEST_DIRECTORY = 'genericAST-test'

# Source: https://stackoverflow.com/questions/58099235/how-to-compare-two-image-files-contents-in-python
def compare_files(file1, file2):
    with open(file1, 'rb') as f1, open(file2, 'rb') as f2:
        contents1 = f1.read()
        contents2 = f2.read()
    return contents1 == contents2   

def compare_all_pngs(compare_dir, test_dir=TEST_DIRECTORY):
    different = []
    for root, _, files in os.walk(compare_dir):
        for fname in files:
            if os.path.splitext(fname)[1] == ".png":
                test_path = os.path.join(test_dir, fname)
                if not os.path.exists(test_path):
                    print(f"{fname} could not be found in test_directory")
                elif not compare_files(test_path, os.path.join(root, fname)):
                    different.append(fname)
    return sorted(different)

print(compare_all_pngs("genericAST-1"))
