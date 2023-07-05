 

# GOSI9_eORCA12-EXPREF
___

[Purpose](#purpose)  |  [Contact](#contact)  |  [License](#license)  |  [Configuration](#configuration) | [Input Files](#input-files)  |  [Diagnostics](#diagnostics)  | [Installation](#installation)

____

## Purpose




## Contact




## Terms of Use

**By downloading this repository and using this code you agree to the following conditions.**

The code in this project is based on the [NEMO](http://www.nemo-ocean.eu) software (Copyright (c) Centre National de la Recherche Scientifique CNRS).

The original code as well as the contribution to this code in this project are licensed under the conditions of [CeCILL](http://www.cecill.info). 

The person stated under '*Contact*' above is the owner of the intellectual property rights of these contributions and **must be informed afore** publishing and **must be cited** in every published work that is based completely or partially on the modifications and additional code provided by this configuration.

Usage is at one's own risk. 


## Configuration

|  **Characteristic** | **Specs** |
|-------------- | -------------- | 
| **Working repository** |  | 
| **Nemo-ocean repository** | https://code.metoffice.gov.uk/svn/nemo/NEMO | 
| **Branch** | branches/UKMO/NEMO_4.0.4_mirror | 
| **Nemo-ocean revision** | 14075 | 
| **Components** | OCE ICE | 
| **Reference Configuration** |  | 
| **CPP keys** | ['key_si3 key_mpp_mpi key_nosignedzero key_iomput'] | 
| **Grid** | eORCA12 | 
| **Resolution** | 12 | 
| **Horizontal Gridpoints** |  | 
| **Vertical Levels** |  | 
| **Atmospheric Condition** | Bulk formulae formulation + separate runoff | 
| **Time Step [s]** | 600.0 | 
| **Passive Tracers** |  | 
| **Number of Nests** |  | 


## Experiment/Simulation

<!--//DELDEL
The default settings for this experiment can be found in the [EXPREF](EXPREF) folder.
The modified code is located in the [MY_SRC](MY_SRC) directory.
DELDEL//-->

### Input Files

*  **NEMO Input File:** File names as they are expected by NEMO  
*  **Reference:** Citation for an article or report, webpage or even better: DOI  
*  **Download:** Link for direct downloading the file (no user-interaction preferred to make it script-compliant)  


| **NEMO Input File** | **Reference (DOI)** | **Download** |
| ------------------ | ------------------ | ------------------ | 
| temperature_clim.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/EN.4.1.1.f.analysis.g10.1995-2014.eORCA12.temperature.teos10.nc) |
| subbasins.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/subbasinmask_eORCA12_v2.4_CMIP6.nc) |
| sss_1m.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/EN.4.1.1.f.analysis.g10.1995-2014.eORCA12.SSS.teos10.nc) |
| shlat2d.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/shlat2d_eORCA12_southps.nc) |
| salinity_clim.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/EN.4.1.1.f.analysis.g10.1995-2014.eORCA12.salinity.teos10.nc) |
| runoff_1m_nomask.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/eORCA12_runoff_GO6_icb.nc) |
| mask_itf.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/eORCA12_msk_itf.nc) |
| geothermal_heating.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/Goutorbe_ghflux.nc) |
| domain_cfg.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/domaincfg_eORCA12_v3.0.nc) |
| calving.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/eORCA12_calving_v2.4.nc) |
| bfr_coef.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/eORCA12_bfr2d_UKmod.nc) |
| M2rowdrg.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/eORCA12_M2_lbclnk_corrected.nc) |
| K1rowdrg.nc |   | [download](https://gws-access.jasmin.ac.uk/public/jmmp/GOSI9_eORCA12_config/eORCA12_inputs/eORCA12_K1_lbclnk_corrected.nc) |



## Diagnostics

See [DIAG](DIAG) for some standard diagnostics from a simulation with this configuraton.

## Installation

There are plenty of ways how to install a local copy of this configuration:

1. You can [clone it with git](#install-with-git) (regardless of whether your NEMOGCM path is already under git control or not). 
2. Or you just download an archive from the web interface.  

In some cases there are different versions of the same configuration in separate branches (e.g. to reflect different NEMO revisions); **check the branches/tags** menu on the web interface or use the git branch and checkout commands to select 
the version you're interested in.


## Install with git


#### (A) NEMOGCM not under git control

If your NEMO installation **is not under git control already**, you can clone this configuration using the URL specified on the project's front page:

Go into the **configurations directory** in your local NEMO installation (`CONFIG/` for NEMO version 3, `cfgs/` for version 4) and clone this project (see the "Clone" link or button on the git webinterface to get the URL).

**EXAMPLE:** In this example, NEMO (version 4) has been installed on a separate scratch-disk (`$WORK`) and the simulation repository was hosted on github under the namespace `$GITNAMESPACE`:

~~~bash
cd $WORK/NEMO-release-4.0/cfgs
git clone git@github:${GITNAMESPACE}}/GOSI9_eORCA12-EXPREF.git
cat GOSI9_eORCA12-EXPREF/EXPREF/exp_cfg.txt >> ./work_cfgs.txt
~~~

You can also specify a certain branch when cloning (e.g. `release-4.0` if it exists):

~~~bash
git clone -b release-4.0 git@github:${GITNAMESPACE}}/GOSI9_eORCA12-EXPREF.git
~~~

This wil create a new configuration folder, which can be used as a reference case for **`makenemo -r`**. 
Make sure, you add this configuration  to the local registry file `cfg.txt` (NEMO version 3) or `work_cfgs.txt` (NEMO version 4) before invoking **`makenemo`**.



#### (B) NEMOGCM already under git control

If your **NEMOGCM installation is already under git control** you cannot clone a different repository into the existing working copy. 
Instead, you can use **`git subtree`** to inject files from another remote repository into a particular sub-folder of your existing working tree.

Within NEMOGCM directory:

**Option - With git commands**

~~~bash
cd $WORK/NEMO-release-4.0
git remote add -f remote_GOSI9_eORCA12-EXPREF git@:/GOSI9_eORCA12-EXPREF.git   # add remote
git subtree add --prefix CONFIG/GOSI9_eORCA12-EXPREF remote_GOSI9_eORCA12-EXPREF branches/UKMO/NEMO_4.0.4_mirror --squash     # donwload master branch into sub-folder
cat cfgs/GOSI9_eORCA12-EXPREF/EXPREF/exp_cfg.txt >> cfgs/work_cfgs.txt
~~~

> In this case, you keep the information from where you have downloaded the reference configuration (see \`git remote -v\`).

Or even shorter, without keeping remote source information (not recommended):

~~~bash
cd $WORK/NEMO-release-4.0
git subtree add --prefix cfgs/GOSI9_eORCA12-EXPREF git@:/GOSI9_eORCA12-EXPREF.git branches/UKMO/NEMO_4.0.4_mirror --squash
cat cfgs/GOSI9_eORCA12-EXPREF/EXPREF/exp_cfg.txt >> cfgs/work_cfgs.txt
~~~


#### Other revisions

The revision that will be installed, is the most recent one from the **master** branch. 
If you're seeking another branch/revision of this configuration (e.g. an older one), you can browse available branches/tags via the web-interface or list alternative 
branches on the command line and swap available branches/tags easily with \`checkout\`:

~~~bash
cd GOSI9_eORCA12-EXPREF
git branches -r
git checkout otherBranch
~~~

Note: *origin/HEAD* in the output listing is not a branch in its own but points to the default branch (master branch in most cases).

        