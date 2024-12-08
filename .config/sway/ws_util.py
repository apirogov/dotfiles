#!/usr/bin/env python
# from gist: https://gist.github.com/dedsm/610a004d896cac3a93c59558a8326dd9
import argparse
from i3ipc import Connection

sway = Connection()
outputs = [o for o in sway.get_outputs() if o.active]


def focus_monitor(args):
    indices = {"left": 0, "center": 1, "right": 2}
    orientation = indices[args.orientation]
    if len(outputs) <= 1 or len(outputs) < orientation+1:
        print(len(outputs))
        return

    sorted_outputs = sorted(outputs, key=lambda x: x.rect.x)
    output = sorted_outputs[orientation]

    sway.command(f"workspace {output.current_workspace}")


def send_to_monitor(args):
    indices = {"left": 0, "center": 1, "right": 2}
    orientation = indices[args.orientation]
    if len(outputs) <= 1 or len(outputs) < orientation+1:
        print(len(outputs))
        return

    sorted_outputs = sorted(outputs, key=lambda x: x.rect.x)
    output = sorted_outputs[orientation]

    sway.command(f"move container to workspace {output.current_workspace}")


def switch_workspace(args):
    workspaces = sway.get_workspaces()
    workspaces_by_name = {w.name: w for w in workspaces}
    focused_workspace = next((w for w in workspaces if w.focused))
    destination = args.workspace

    if focused_workspace.name == destination:
        return

    if destination not in workspaces_by_name:
        # workspace doesn't exist
        sway.command(f"workspace {destination}")
        return

    target_workspace = workspaces_by_name[destination]
    destination_output = focused_workspace.output
    source_output = target_workspace.output
    if destination_output == source_output:
        sway.command(f"workspace {destination}")
        return

    # workspace belongs to a different output
    if target_workspace.visible:
        # the workspace we want in the current output is
        # being displayed in another output, so we swap
        commands = [
            "workspace ___temp___",
            f"move workspace to output {source_output}",
            f"workspace {destination}",
            f"move workspace to output {destination_output}",
            f"workspace {focused_workspace.name}",
            f"move workspace to output {source_output}",
            f"workspace {destination}",
        ]
        command = ";".join(commands)
        sway.command(command)
    else:
        # the workspace is in a different output but hidden
        # bring it to the destination output, no need for temp
        # since we know there's at least one more
        commands = [
            f"workspace {destination}",
            f"move workspace to output {destination_output}"
        ]
        command = ";".join(commands)
        sway.command(command)


parser = argparse.ArgumentParser(description="Process custom sway commands")

subparsers = parser.add_subparsers(help="sub-command help")

output_choose = subparsers.add_parser(
    "monitor",
    help="pick monitor in X orientation order"
)
output_choose.add_argument(
    "orientation",
    type=str,
    choices=["left", "center", "right"]
)
output_choose.set_defaults(func=focus_monitor)

send_to_output = subparsers.add_parser(
    "send_to_output",
    help="pick monitor in X orientation order"
)
send_to_output.add_argument(
    "orientation",
    type=str,
    choices=["left", "center", "right"]
)
send_to_output.set_defaults(func=send_to_monitor)

workspace_choose = subparsers.add_parser(
    "workspace",
    help="switch workspace"
)
workspace_choose.add_argument(
    "workspace",
    type=str
)
workspace_choose.set_defaults(func=switch_workspace)


args = parser.parse_args()
args.func(args)
