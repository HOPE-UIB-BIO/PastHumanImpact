# Cluster computation walkthrough

## 1. Make jobs

Run script `/R/hypothesis_2/run_h2.r`. There should be automated job creation for both SLURM and TORQUE.

## 2. Connect to Bayreuth VPN

Follow the [online instructions](https://www.its.uni-bayreuth.de/en/internet-und-email/index.html).

## 3. Connect to cluster

Use SSH to connect to a cluster.

For example, `ssh <bt715577@btrzx1-2.rz.uni-bayreuth.de` will connect to the cluster 1-2.

VS Code has a build-in extension for this: Remote Tunnels

## 4. Prepare files

Copy all files into the `LA` folder.

There is a step to make sure that the formatting is correct.

Workaround:

```bash
zip -r -ll zipfile.zip LA
unzip zipfile.zip
```

## 5. Submit jobs

Depending on the type of cluster select SMURM (btrzx1) or TORQUE ( btrzx2, btrzx4, btrzx3, and btrzx5)

copy all text from either `qsub_all.txt` or `sbatch_all.txt` and place into console

## Checking status

 `qstat -u bt715577` (Torque) or `squeue -u bt715577`  (Slurm)
