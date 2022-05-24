#! /usr/bin/env python

import argparse
import os
from bs4 import BeautifulSoup
import shutil
import requests

def download_file(url, root_dir, force_overwrite=False):
    """
    Downloads a file from a provided url into a root directory.
    :param url:
    :param root_dir:
    :return:
    """
    local_filename = url.split('/')[-1]
    session = requests.Session()
    session.auth = ('foo', 'bar')

    with session.get(url, stream=True) as r:
        if not os.path.isfile(os.path.join(root_dir, local_filename)) or force_overwrite:
            try:
                with open(os.path.join(root_dir, local_filename), 'wb') as f:
                    shutil.copyfileobj(r.raw, f)
            except IsADirectoryError:
                print("Ignoring directory.")
        else:
            print("Local file exists. Skipping download.")
    return local_filename


def download_dir(url, root_dir, force_overwrite=False):
    session = requests.Session()
    session.auth = ('foo', 'bar')

    resp = session.get(url)
    soup = BeautifulSoup(resp.content, features='lxml')
    forcing_files = [link.get("href") for link in soup("a")]
    if not os.path.isdir(root_dir):
        os.mkdir(root_dir)
    for filename in forcing_files[1:]:
        # only try to download netcdf files
        if filename.endswith('nc'):
            print("...downloading surface forcing file '{}'".format(filename))
            download_file(os.path.join(url, filename), root_dir, force_overwrite=force_overwrite)


def download_ancils(config_dir=None, target_dir=None, download_forcing=False,
                    force_overwrite=False):
    """
    Downloads ancillary files from locations listed in the input.ini file.
    :param ini_file:
    :param target_dir:
    :return:
    """
    input_def = os.path.join(config_dir, 'input.def')
    with open(input_def) as fp:
        for line in fp:
            z = line.split(',')
            url = z[2].strip()
            target = z[0].strip()
            if target == "SURFACE_FORCING_DIR":
                if download_forcing:
                    target = url.split('/')[-1].strip()
                    print("Downloading surface forcing dir {} from {}".format(target, url))
                    download_dir(url, target_dir+'/'+target,force_overwrite=force_overwrite)
            else:
                print("Downloading {} from {}".format(target, url))
                download_file(url, target_dir, force_overwrite=force_overwrite)

if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config_dir",action="store",dest="config_dir",
                    help="name of ini file")
    parser.add_argument("-t", "--target_dir",action="store",dest="target_dir",
                    help="name of download directory")
    parser.add_argument("-f", "--download_forcing",action="store_true",dest="download_forcing",
                    help="download the surface forcing directory")
    parser.add_argument("-X", "--force_overwrite",action="store_true",dest="force_overwrite",
                    help="overwrite pre-existing local files")

    args = parser.parse_args()
    download_ancils(config_dir=args.config_dir, download_forcing=args.download_forcing, 
                    target_dir=args.target_dir, force_overwrite=args.force_overwrite )
