# Path to Oh My Fish install.
set -gx OMF_PATH "/home/tuomo/.local/share/omf"

# Customize Oh My Fish configuration path.
#set -gx OMF_CONFIG "/home/tuomo/.config/omf"

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

# Shortcut for todo.txt
function t
  bash /home/tuomo/Dropbox/todo/todo.sh -d /home/tuomo/Dropbox/todo.cfg $argv
end

source ~/.config/fish/nvm-wrapper/nvm.fish
