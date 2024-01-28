import dataclasses
import enum
import math

from typing import Union, Callable
from collections import deque


class Pulse(enum.Enum):
    LOW = 0,
    HIGH = 1,


@dataclasses.dataclass
class FlipFlop:
    name: str
    connections: list[str]
    state: bool = False

    def receive(self, broadcaster: 'Broadcaster', sender: str, pulse: Pulse):
        if pulse == Pulse.LOW:
            to_send = Pulse.LOW if self.state else Pulse.HIGH
            self.toggle()
            broadcaster.broadcast(self.name, to_send, self.connections)

    def toggle(self):
        self.state = not self.state


@dataclasses.dataclass
class Conjunction:
    name: str
    connections: list[str]
    # needs to be initialized with all __inputs__ to Pulse.LOW later
    # can't use {} as default since it's mutable and would be inited on the
    # class not the instance, so we have to use a default factory
    memory: dict[str, Pulse] = dataclasses.field(default_factory=dict)

    # NOTE: memory is for inputs, but only sends pulses to connected/outputs
    # # InitVar is not initialized by the generated init, but
    # # it needs to be passed as param and is forwarded to
    # # __post_init__
    # connections: dataclasses.InitVar[list[str]]
    # memory: dict[str, Pulse] = dataclasses.field(init=False)

    # def __post_init__(self, connections: list[str]):
    #     # remember low pulse initially
    #     self.memory = {name: Pulse.LOW for name in connections}

    def receive(self, broadcaster: 'Broadcaster', sender: str, pulse: Pulse):
        # update memory first
        self.memory[sender] = pulse
        to_send = self.pulse_to_send()
        broadcaster.broadcast(self.name, to_send, self.connections)

    def pulse_to_send(self) -> Pulse:
        all_high = all(p == Pulse.HIGH for p in self.memory.values())
        if all_high:
            return Pulse.LOW
        else:
            return Pulse.HIGH

    def init_input(self, module_name: str):
        self.memory[module_name] = Pulse.LOW


Module = Union[FlipFlop, Conjunction]


class Broadcaster:

    def __init__(
            self,
            predicate: Callable[[str, Pulse, list[str]], bool] | None = None) -> None:
        # connections of the broadcaster itself
        self.connections: list[str] = []
        # all modules
        self.modules: dict[str, Module] = {}
        # pulses get processed in the order they're sent in
        # so use a FIFO queue
        self.pulses = deque[tuple[str, Pulse, list[str]]]()
        self.pulses_sent = {Pulse.LOW: 0, Pulse.HIGH: 0}
        self.predicate = (
            predicate
            if predicate is not None
            else lambda sender, pulse, receiver_modules: False)

    def set_connections(self, connections: list[str]):
        self.connections = connections

    def set_predicate(self, predicate: Callable[[str, Pulse, list[str]], bool]):
        self.predicate = predicate

    def add_module(self, module: Module):
        # print('addmod', module)
        self.modules[module.name] = module

    def broadcast(self, sender: str, pulse: Pulse, modules: list[str]):
        self.pulses.append((sender, pulse, modules))
        self.pulses_sent[pulse] += len(modules)

    def update_conjunction_inputs(self):
        # Conjunction modules need to know about all their inputs,
        # so their memory function can work correctly
        for module_name, module in self.modules.items():
            for con in module.connections:
                try:
                    target = self.modules[con]
                    if isinstance(target, Conjunction):
                        target.init_input(module_name)
                except KeyError:
                    # NOTE: ignore untyped modules (no connections anyway)
                    pass

    def button(self) -> bool:
        # button low-> broadcaster
        # also needs to be counted
        self.pulses_sent[Pulse.LOW] += 1
        self.broadcast('broadcaster', Pulse.LOW, self.connections)

        predicate_was_true = False
        while len(self.pulses) > 0:
            sender_name, pulse, modules = self.pulses.popleft()
            if self.predicate(sender_name, pulse, modules):
                predicate_was_true = True

            for module_name in modules:
                # print(sender_name, pulse, "->", module_name)
                try:
                    self.modules[module_name].receive(self, sender_name, pulse)
                except KeyError:
                    # NOTE: intignore untyped modules (no connections anyway)
                    pass

        return predicate_was_true


with open("d20_input.txt", "r") as f:
    lines = f.readlines()


def parse_modules(lines: list[str]) -> Broadcaster:
    broadcaster = Broadcaster()
    for line in lines:
        if not line.strip():
            continue
        name, connections_str = line.strip().split(" -> ")
        connections = connections_str.split(", ")

        if name[0] == "%":
            name = name[1:]
            broadcaster.add_module(FlipFlop(name, connections))
        elif name[0] == "&":
            name = name[1:]
            broadcaster.add_module(Conjunction(name, connections))
        else:
            broadcaster.set_connections(connections)
    broadcaster.update_conjunction_inputs()
    return broadcaster


broadcaster = parse_modules(lines)
for _ in range(1000):
    broadcaster.button()

print("Part1:",
      broadcaster.pulses_sent[Pulse.LOW], "*",
      broadcaster.pulses_sent[Pulse.HIGH], "=",
      broadcaster.pulses_sent[Pulse.LOW]*broadcaster.pulses_sent[Pulse.HIGH])

# NOTE: part2 assumptions
# - rx is connected to one conjunction module A (here: qb)
# - that in turn is connected to n other conjunction modules B_i
#   (here 4: kv, jg, rz, mr)
# -> rx will receive a low pulse once all remembered inputs are high for A
#    the inputs B_i will send a high pulse if any of their inputs are
#    remembered as low
# => find B_i modules, find cycles where they send a high pulse,
#    then find the least common multiple (LCM), which is when rx
#    will receive a low pulse


# get the inputs for receiver
def get_inputs(broadcaster: Broadcaster, receiver: str) -> list[str]:
    inputs = []
    for module in broadcaster.modules.values():
        if receiver in module.connections:
            inputs.append(module.name)
    return inputs


# find semicycle where the passed module name sends it's first high pulse
def find_semicycle(module_to_send_high_pulse: str) -> int:
    # need to use original state
    broadcaster = parse_modules(lines)

    def pred(sender: str, pulse: Pulse, receiver_modules: list[str]) -> bool:
        return sender == module_to_send_high_pulse and pulse == Pulse.HIGH

    broadcaster.set_predicate(pred)
    button_presses = 0
    while True:
        pred_true = broadcaster.button()
        button_presses += 1
        if pred_true:
            break

    return button_presses


broadcaster_pt2 = parse_modules(lines)
A = get_inputs(broadcaster_pt2, "rx")[0]
Bs = get_inputs(broadcaster_pt2, A)
button_presses_till_high_pulse = [find_semicycle(mod) for mod in Bs]
high_pulses_at_the_same_time = math.lcm(*button_presses_till_high_pulse)
print("Part2:", high_pulses_at_the_same_time)
