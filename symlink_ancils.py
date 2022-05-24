#! /usr/bin/env python

import argparse
import os

def symlink_ancils(ini_file, download_folder, work_folder):
    """
    Symlinks ancillary files defined in ini_file from download folder to 
    work_folder with correct filenames (as expected by NEMO).
    :param ini_file:
    :param download_folder:
    :param work_folder:
    :return:
    """
    with open(ini_file) as fp:
        for line in fp:
            z = line.split(',')
            input_file = z[2].split('/')[-1].strip()
            target_linkname = z[0]
            if target_linkname = "SURFACE_FORCING_DIR":
                target_linkname = input_file
            print('Linking +'+input_file+'+ to +'+target_linkname+'+')
            print('Source : '+os.path.join(download_folder,input_file))
            print('Target : '+os.path.join(work_folder,target_linkname))
            os.symlink(os.path.join(download_folder,input_file),os.path.join(work_folder,target_linkname))

if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--ini_file",action="store",dest="ini_file",
                    help="name of ini file")
    parser.add_argument("-d", "--download_dir",action="store",dest="download_folder",
                    help="name of download directory")
    parser.add_argument("-w", "--work_dir",action="store",dest="work_folder",
                    help="name of work directory")

    args = parser.parse_args()
    symlink_ancils(args.ini_file, args.download_folder, args.work_folder)
