import os
import re

# Regex patterns
base_pattern = re.compile(r"^(d\d+)\..*|^(d\d+)$")
task_pattern = re.compile(r"^(d\d+).*task.*", re.IGNORECASE)

def find_all_files(root):
    for dirpath, _, filenames in os.walk(root):
        for fname in filenames:
            yield dirpath, fname

def main():
    root = os.getcwd()

    # All top-level directories inside root
    top_dirs = [
        os.path.join(root, d)
        for d in os.listdir(root)
        if os.path.isdir(os.path.join(root, d))
    ]

    for top in top_dirs:
        print(f"\nChecking top-level directory: {os.path.basename(top)}")
        
        found_base = {}  # maps "dXX" -> list of filepaths
        found_task = set()  # set of "dXX" that have task files

        # Walk the directory
        for dirpath, fname in find_all_files(top):

            # Check for base files: ^d\d+\.* (matching d10, d10.txt, d10.v1)
            m = base_pattern.match(fname)
            if m:
                key = m.group(1) or m.group(2)  # extract "dXX"
                found_base.setdefault(key, []).append(os.path.join(dirpath, fname))

            # Check for task files: ^d\d+.*task.*
            m_task = task_pattern.match(fname)
            if m_task:
                key = m_task.group(1)  # extract "dXX"
                found_task.add(key)

        # Report missing task files
        missing = [key for key in found_base.keys() if key not in found_task]

        if missing:
            print("Missing task files for:")
            for key in missing:
                print(f"  {key}:")
                for f in found_base[key]:
                    print(f"    {f}")
        else:
            print("All files have corresponding task files ✔")

if __name__ == "__main__":
    main()
