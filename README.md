Exchanges

Bitcoin Donations: 1GbpRPE83Vjg73KFvTVZ4EnS2qNkiLY5TT

Allows your computer to control multiple bitcoin exchanges. Gives CSV (comma seperated values) files of data from the exchanges so it is easy to export to other software. Allows for TWAT (time weighted average trading)

This software is not yet finished. Use at your own risk. If you want to contribute, have a look at the todo.md file.

Here is an example output that is currently possible using the main:price() function.

```
exchange, buy, high, last, low, sell, vol,
btce, 431.14, 433.50, 431.15, 429.05, 431.02, 5838.79,
itbit, 432.24, 434.88, 432.46, 429.51, 432.46, 1793.18,
bitstamp, 432.30, 435.86, 432.67, 429.50, 432.67, 5621.20,
bitfinex, 431.70, 435.67, 431.86, 427.91, 431.95, 15545.39,
btcchina, 2830.75, 2852.29, 2831.60, 2810.84, 2831.60, 47378.02,
```

To use this software make sure you have erlang installed. Version 18 is prefered, but older versions will probably work. 

Here is one way to download it: http://www.erlang.org/download.html 

here are erlang install instructions: http://www.erlang.org/doc/installation_guide/INSTALL.html

For ubuntu, I needed to install dependencies before I could install erlang:

```
sudo apt-get install libncurses5-dev
sudo apt-get install libssl-dev
sudo apt-get install unixodbc-dev
```

Once you have erlang you can install exchanges:

```
sh install.sh  #We need to make a seperate script for windows users. For now, see windows_install.md
```
And then you can run the software:

```
sh start.sh  #We need a seperate script for windows users.
```


