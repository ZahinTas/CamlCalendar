# Install Instructions

# In your terminal run the commands one by one:

git clone git@github.coecis.cornell.edu:zt276/CamlCalendar.git
cd CamlCalendar

opam update
opam upgrade
opam install calendar
opam install dune yojson

# If on macOS:
brew install mutt

# OR If on Ubuntu/Debian:
sudo apt-get install mutt

# OR If on other Linux:
sudo yum install mutt    # RHEL/CentOS
sudo pacman -S mutt      # Arch

# Run the following commands after
dune build
dune exec bin/main.exe

# If there are any missing dependencies, run: 
opam install yojson unix dune

# To clean and rebuild: 
dune clean && dune build