
# Get a French Word List

- Log into the VM
- `$ which aspell`. Did you get `/usr/bin/aspell`?
- If not, `$ sudo apt-get install aspell -y`
- Get a French dictionary: `$ sudo apt-get install aspell-fr -y`
- Create a directory for your validation data:
	- `$ cd /vagrant/`
	- `$ mkDir validation-data`
- Navigate into it: `$ cd validation-data`
- Do what it takes to dump the aspell French dictionary into a basic word-list:
	- `$ aspell -d fr dump master | aspell -l fr expand > words.txt`
- Confirm you have a lot of words: `$ less words.txt` (`q` to get out of `less`).
- Find out how many words you have: `$ wc -l words.txt`.
