with open("d01.in", "r") as f:
    lines = f.readlines()
#lines = ["12", "14", '1969', '100756']
total_fuel = 0
for ln in lines:
    mass = int(ln.strip())
    fuel_req = (mass // 3) - 2
    total_fuel += fuel_req

print("PART1:", total_fuel)

# PART2
total_fuel = 0
for ln in lines:
    mass = int(ln.strip())
    fuel_req = (mass // 3) - 2
    total_fuel += fuel_req
    while True:
        fuel_req = (fuel_req // 3) - 2
        if fuel_req <= 0:
            break
        total_fuel += fuel_req

print("PART2:", total_fuel)
