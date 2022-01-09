import re

from utils import readfile

inp = readfile("d24.in").strip().splitlines()
# inp = """Immune System:
# 17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
# 989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

# Infection:
# 801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
# 4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4""".splitlines()

LINERE = re.compile(r"(\d+) units each with (\d+) hit points(?: \((.+)\))? with an "
                    r"attack that does (\d+) (\w+) damage at initiative (\d+)")
IMM_SYS, INFECTION = 0, 1


class Group:
    def __init__(self, t, unit_count, hpu, atk, atk_type, ini):
        self.type = t
        # since only whole units are killed and any surplus dmg is ignored
        # we only have to keep track of the units and the hp per unit
        self.unit_count = int(unit_count)
        self.hp_per_unit = int(hpu)
        self.atk = int(atk)
        self.atk_type = atk_type.strip()
        self.initiative = int(ini)
        self.def_info = {}
        self.defending = False
        self.target = None

    @property
    def effective_power(self):
        return self.unit_count * self.atk

    @property
    def hp(self):
        return self.hp_per_unit * self.unit_count

    def calc_dmg(self, other):
        try:
            return self.effective_power * other.def_info[self.atk_type]
        except KeyError:
            return self.effective_power

    def choose_target(self, groups):
        # make sure we have no target at start
        self.target = None
        max_dmg = 0
        for grp in groups:
            if grp.type == self.type or grp.defending:
                continue
            else:
                dmg = self.calc_dmg(grp)
                if dmg <= 0:
                    # we can have no target if we cant deal more than 0 dmg
                    continue
                elif max_dmg < dmg:
                    self.target = grp
                    max_dmg = dmg
                elif max_dmg == dmg:
                    if self.target is None or self.target < grp:
                        self.target = grp
                        max_dmg = dmg
        if self.target is not None:
            # set defending -> cant be chosen by other grp
            self.target.defending = True

    def attack(self):
        if self.unit_count <= 0:
            self.target.defending = False
            return 0
        dmg = self.calc_dmg(self.target)
        # we can optionally make sure we report killed units only up to the targets unit count
        # units_killed = min(self.target.unit_count, dmg // self.target.hp_per_unit)
        units_killed = dmg // self.target.hp_per_unit
        self.target.unit_count -= units_killed
        # reset defending so it can be chosen as target next round
        self.target.defending = False
        return units_killed

    # For all six of the above operators, if __cmp__ is defined on the
    # left-hand argument, it is called with the right-hand argument. A result
    # of -1 indicates the LHS is less than the RHS. A result of 0 indicates
    # they are equal. A result of 1 indicates the LHS is greater than the RHS.
    # less than
    def __lt__(self, other):
        # grp with higher eff pow first
        if self.effective_power < other.effective_power:
            return True
        elif self.effective_power == other.effective_power:
            # same eff pow then grp with higher initiative
            return self.initiative < other.initiative
        else:
            return False

    def __str__(self):
        return f"Group<{'IS' if self.type == IMM_SYS else 'INF'} HP: {self.hp} HPU: {self.hp_per_unit}, UNI: {self.unit_count} ATK: {self.atk}, ATK_TYPE: {repr(self.atk_type)}, POW: {self.effective_power} INI: {self.initiative}, DEF: {', '.join(f'{k}: {v}' for k,v in sorted(self.def_info.items()))}>"

    def __repr__(self):
        return self.__str__()

    def __copy__(self):
        newone = Group("adas", 0, 0, 0, "asa", 0)
        newone.__dict__.update(self.__dict__)
        newone.def_info = {k: v for k, v in self.def_info.items()}
        return newone


groups_orig = []
grp_type = -1
for ln in inp:
    if not ln:
        continue
    elif ln[-1] == ':':
        grp_type += 1
        continue
    m = LINERE.match(ln)
    new_grp = Group(grp_type, m.group(1), m.group(2), m.group(4), m.group(5), m.group(6))
    if m.group(3):
        for def_info in m.group(3).split(';'):
            if def_info.strip().startswith("imm"):
                immu = def_info.strip().replace("immune to ", "")
                for immu_type in immu.split(", "):
                    # dmg ignored when immune so multiply by def_info[atk_type] -> 0
                    new_grp.def_info[immu_type.strip()] = 0
            else:
                weak = def_info.strip().replace("weak to ", "")
                for weak_type in weak.split(", "):
                    # weak -> double dmg
                    new_grp.def_info[weak_type.strip()] = 2
    groups_orig.append(new_grp)
    # print(new_grp)

added_dmg = 0
while True:
    groups = [g.__copy__() for g in groups_orig]
    for g in groups:
        if g.type == IMM_SYS:
            g.atk += added_dmg

    # just set to 1 so start condition for while loop is true
    imm_alive = 1
    inf_alive = 1
    stalemate = False
    while imm_alive > 0 and inf_alive > 0:
        # need biggest (highest eff pow etc) first
        groups.sort(reverse=True)
        assert all(g.unit_count > 0 for g in groups)
        for grp in groups:
            grp.choose_target(groups)

        # check if no units were killed -> otherwise stalemate
        units_killed = False
        # attack order is based on highest initiative
        groups.sort(key=lambda x: x.initiative, reverse=True)
        for grp in groups:
            # no target or grp has no units to attack with
            # BUG: i had  or grp.unit_count <= 0: here which caused the grp to be skipped
            # if it doesnt have units to attack with but contrary to the grp's attack function
            # it wouldn't reset defending to False on the target which wouldn't make it available
            # as a target the next round!!!!!!!!!!!!!!!!!!
            if grp.target is None:
                continue
            # print(grp)
            # print("attacking")
            # print(grp.target)
            uk = grp.attack()
            if uk > 0:
                units_killed = True
            # print("KILLED:", uk)
            # print("=======================")
        if not units_killed:
            stalemate = True
            print("STALEMATE")
            # stalemate
            break

        # sort out dead groups
        groups = [g for g in groups if g.unit_count > 0]
        # just to be extra sure
        for g in groups:
            # reset target and defending (so grp is available as target the next round)
            # this is already done b4 but i had a bug previously where i forgot to
            # reset defending on the target when the attacking grp didnt have units
            # so just making sure
            g.target = None
            g.defending = False
        # print("+++++++++GROUPS+++++++++++")
        # print("\n".join(str(g) for g in groups))
        # print("++++++++++++++++++++++++++")
        # print("Units alive:", sum(g.unit_count for g in groups))
        imm_alive = sum(g.unit_count for g in groups if g.type == IMM_SYS)
        inf_alive = sum(g.unit_count for g in groups if g.type == INFECTION)

    if added_dmg == 0:
        print("Part1:", imm_alive or inf_alive)
    elif imm_alive and not stalemate:
        print("Part2:", imm_alive, f"(at {added_dmg} added damage)")
        break
    elif not stalemate:
        print("Infection won with:", sum(g.unit_count for g in groups), "units alive at", added_dmg, "added dmg")
    added_dmg += 1
