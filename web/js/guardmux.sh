#!/bin/bash

# Root of the git repository
repo_root=$(git rev-parse --show-toplevel)

# Start new session
tmux new-session -d -s guardian

# Split the window into four panes
tmux split-window -h 
tmux split-window -v 
tmux select-pane -t 0
tmux split-window -v 

# Change directory in each pane
tmux send-keys -t guardian:0.0 "cd $repo_root/DbSync/scripts ; ./localrun.sh" C-m
tmux send-keys -t guardian:0.1 "cd $repo_root/DbAnalyzer/scripts; ./localrun.sh" C-m
tmux send-keys -t guardian:0.2 "cd $repo_root/DbGuardian/scripts; ./localrun.sh" C-m
tmux send-keys -t guardian:0.3 "cd $repo_root/DbGuardian/scripts" C-m

# Attach to the new session
tmux attach -t guardian