export PATH=/Users/tuomo.virolainen/bin:/usr/local/bin:/Users/tuomo.virolainen/mongodb/mongodb-osx-x86_64-3.6.6/bin:/Users/tuomo.virolainen/anaconda3/bin:/Users/tuomo.virolainen/dev/yle-aws-tools/bin:$PATH

export AWS_ASSUME_ROLE_NAME=dev
export AWS_ASSUME_ROLE_ARN=arn:aws:iam::352476883983:role/dev
export BAT_THEME="ansi-dark"

source /usr/local/opt/chruby/share/chruby/chruby.sh
source /usr/local/opt/chruby/share/chruby/auto.sh
chruby 2.5.1
eval "$(yle complete zsh)"

export PATH="$HOME/.cargo/bin:$PATH"

# Setting PATH for Python 3.6
# The original version is saved in .zprofile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
export PATH
