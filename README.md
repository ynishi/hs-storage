# hs-storage

## required
* zfs
* smartctl

## install bin
```
stack install
# mv under global path
sudo mv ${exec_path} /usr/local/bin/.
# check
sudo /usr/local/bin/hs-storage-exe -h
```
## run
### usage
```
sudo stack run --allow-different-user -- -h
```
### sample
```
sudo stack run --allow-different-user -- backup --dryrun
```
