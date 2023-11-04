import re
from collections import defaultdict

def get_input(file_name):
    with open(file_name) as f:
        contents = f.read()
    valves = {}
    valve_connections = defaultdict(list)
    for ln in contents.splitlines():
        valve_name = ln[6:9]
        rate_end = ln.find(";")
        rate = int(ln[23:rate_end])
        valves[valve_name] = rate

        grps = re.search(r"valves? (.*)$", ln)
        # matches = re.match(r"Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)$", ln)
        for to_valve in grps.group(1).split(", "):
            valve_connections[valve_name].append(to_valve)
    return valves, valve_connections


def main():
    file_name = "d16.in"
    valves, valve_connections = get_input(file_name)
    best = (max(valves.values()))

    

if __name__ == "__main__":
    main()
