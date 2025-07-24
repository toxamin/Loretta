#!/usr/bin/env python3
import argparse

def main():
    parser = argparse.ArgumentParser(
        description="Remove any lines in INPUT that appear in REMOVE."
    )
    parser.add_argument(
        "input_file",
        help="Path to the file you want to filter"
    )
    parser.add_argument(
        "remove_file",
        help="Path to the file containing lines to remove"
    )
    parser.add_argument(
        "output_file",
        help="Path where the filtered output will be written"
    )
    args = parser.parse_args()

    # Load all lines to remove (stripped of trailing newline)
    with open(args.remove_file, 'r', encoding='utf-8') as rf:
        to_remove = { line.rstrip('\n') for line in rf }

    # Stream through input_file and write only lines not in to_remove
    with open(args.input_file, 'r', encoding='utf-8') as inf, \
         open(args.output_file, 'w', encoding='utf-8') as outf:
        for line in inf:
            if line.rstrip('\n') not in to_remove:
                outf.write(line)

if __name__ == "__main__":
    main()