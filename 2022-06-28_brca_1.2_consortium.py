"""This is a request from MSK as they wanted the BrCa 1.2-consortium
release to be exactly the same as the 1.1-consortium release.

1. Modify synapsePythonClient to allow for copying of files with access
restrictions and install from source.

    synapse cp syn26253353 --destinationId syn32298950 --setProvenance None
    synapse cp syn24981909 --destinationId syn32298950 --setProvenance None

2. Update release_version column in the clinical files
"""
import pandas as pd
import synapseclient

syn = synapseclient.login()

# Update release_version
release_12_files = syn.getChildren("syn32299038")
for each_file in release_12_files:
    ent = syn.get(each_file['id'])
    ent_df = pd.read_csv()