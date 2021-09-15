# Correct all GTFS feeds

import json
import shutil
import datetime as dt
import zipfile
import pandas as pd
from io import BytesIO
import os
import numpy as np

# Set defaults
#os.mkdir("data/gtfs-clean")
agency_count = 0
pd.options.mode.chained_assignment = None  # Turn off set with copy warnings


# Check GTFS files for errors and fix them


def unzip_to_file(zfile, bpath, full_path, stop_dataset=None, transfer_dataset=None, agency_dataset=None, trips_dataset=None, caldate_dataset=None, routes_dataset=None, st_dataset=None, pathways_dataset=None):
    # Description: Unzips a zip file to folder, flattens file format, and zips back up
    #   Optional arguments to remove lines from file that cause errors
    # Modified from https://stackoverflow.com/questions/4917284/extract-files-from-zip-without-keeping-the-structure-using-python-zipfile
    # Input:
    #   - zfile: a zipfile archive
    #   - bpath: base path to unzip files
    #   - full_path: full path to zip archive
    #   - stop_dataset: pandas dataframe of the stop_times.txt stop_dataset
    #   - transfer_dataset: pandas dataframe of the transfers.txt dataset
    #   - agency_dataset: pandas dataframe of the agency.txt dataset
    #   - trips_dataset: pandas dataframe of the trips.txt dataset
    #   - caldate_dataset: pandas dataframe of the calendar_dates.txt dataset
    #   - routes_dataset: pandas dataframe of the routes.txt dataset
    #   - st_dataset: pandas dataframe of the stops.txt dataset
    # Output: None
    global agency_count
    all_names = []
    for member in zfile.namelist():
        filename = os.path.basename(member)
        # skip directories
        if not filename:
            continue
        # copy or correct file (taken from zipfile's extract)
        source = zfile.open(member)
        if filename == "stop_times.txt" and stop_dataset is not None and st_dataset is None:  # interpolate stop times
            interp_stop_times(stop_dataset, filename)
        elif filename == "stop_times.txt" and stop_dataset is not None and st_dataset is not None:  # Remove invalid stop_id from stop_times.txt
            all_stop_ids = st_dataset["stop_id"].unique().tolist()
            stop_dataset = stop_dataset.loc[stop_dataset["stop_id"].isin(all_stop_ids)]
            stop_dataset.to_csv('stop_times.txt', index=False)
        elif filename == "transfers.txt" and transfer_dataset is not None:  # set transfer_type to 0 for missings
            transfer_dataset["transfer_type"].loc[transfer_dataset["transfer_type"].isnull()] = 0
            transfer_dataset["transfer_type"] = transfer_dataset["transfer_type"].astype(int)
            transfer_dataset.to_csv('transfers.txt', index=False)
        elif filename == "agency.txt" and agency_dataset is not None:  # fix missing fields in agency.txt
            agency_dataset["agency_timezone"].loc[agency_dataset["agency_timezone"].isnull()] = 'America/Chicago'
            agency_dataset["agency_url"].loc[agency_dataset["agency_url"].isnull()] = "https://developers.google.com/transit/gtfs/reference/"
            agency_dataset["agency_name"].loc[agency_dataset["agency_name"].isnull()] = "Agency{}".format(agency_count)
            agency_count += 1
            agency_dataset.to_csv('agency.txt', index=False)
        elif filename == "trips.txt" and trips_dataset is not None and routes_dataset is not None:  # fix duplicates in trips.txt, Add route_ids not in routes.txt file
            trips_dataset = trips_dataset.drop_duplicates('trip_id')
            route_ids = routes_dataset["route_id"].values.tolist()
            trips_dataset_un = trips_dataset["route_id"].loc[~trips_dataset["route_id"].isin(route_ids)].unique()
            route_type = routes_dataset["route_type"].mode().tolist()[0]  # All new routes will have the same type as the most common route
            routes_cols = []
            if "route_short_name" in routes_dataset.columns:
                routes_cols.append("route_short_name")
            if "route_long_name" in routes_dataset.columns:
                routes_cols.append("route_long_name")
            if "agency_id" in routes_dataset.columns:
                agency_id = routes_dataset["agency_id"].mode().tolist()[0]
            store_data = []
            for k in range(len(trips_dataset_un)):
                temp = {"route_id": trips_dataset_un[k], "route_type": route_type}
                for l in range(len(routes_cols)):
                    temp[routes_cols[l]] = "{}{}".format(routes_cols[l], k)
                if "agency_id" in routes_dataset.columns:
                    temp["agency_id"] = agency_id
                store_data.append(temp)
            routes_dataset = routes_dataset.append(pd.DataFrame(store_data), ignore_index=True, sort=True)
            routes_dataset.to_csv('routes.txt', index=False)
            trips_dataset.to_csv('trips.txt', index=False)
        elif filename == "routes.txt" and trips_dataset is not None and routes_dataset is not None:  # We've already written out routes.txt, so don't overwrite
            pass
        elif filename == "calendar_dates.txt" and caldate_dataset is not None:  # fix non-allowed date in calendar dates
            caldate_dataset["date"] = caldate_dataset["date"].astype(int)
            caldate_dataset = caldate_dataset.loc[caldate_dataset["date"] > 10000]
            caldate_dataset.to_csv('calendar_dates.txt', index=False)
        elif filename == "routes.txt" and routes_dataset is not None:  # Fix duplicates in routes.txt
            routes_dataset = routes_dataset.drop_duplicates('route_id')
            routes_dataset.to_csv('routes.txt', index=False)
        elif filename == "pathways.txt" and pathways_dataset is not None:
            path_type, pathways_dataset_clean = fix_pathways(pathways_dataset)
            if path_type:
                pathways_dataset_clean.to_csv('pathways.txt', index=False)
        else:  # If we're not replacing lines, just copy the file directly
            with open(filename, "wb") as target:
                shutil.copyfileobj(source, target)
        all_names.append(filename)
    with zipfile.ZipFile(full_path, "w", zipfile.ZIP_DEFLATED) as zip_out:
        for member in all_names:
            zip_out.write(member)
    for member in all_names:
        os.remove(member)


def clean_zipfile(path):
    # Description: For zipfiles that do not have txt files in the root, fix them
    #     Errors include a zip in a zip, and a zipped up folder
    # Input: path to zipfile, relative or absolute
    # Output: zipfile archive
    shutil.copyfile(path, path.replace('gtfs-raw', 'gtfs-clean'))
    path = path.replace('gtfs-raw', 'gtfs-clean')
    archive = zipfile.ZipFile(path)
    path0 = "/".join(path.split('/')[:-1])
    # Check for error where it is a zipfile in a zipfile, and unzip it
    if archive.namelist()[0].split('.')[-1] == 'zip':
        print("  Fixing zipfile in a zipfile")
        zfiledata = BytesIO(archive.read(archive.namelist()[0]))
        archive = zipfile.ZipFile(zfiledata)
        unzip_to_file(archive, path0, path)
    # Resolve where agency puts files in a subfolder
    is_subdir = False
    if 'stops.txt' not in archive.namelist():
        for i in archive.namelist():
            if 'stops.txt' in i:
                is_subdir = True
                break
    if is_subdir:
        print("  Fixing files placed in a subfolder")
        unzip_to_file(archive, path0, path)
    # Remove invalid stop_ids from stop_times.txt
    archive = zipfile.ZipFile(path)
    st_data = archive.open('stops.txt')
    stops_data = archive.open('stop_times.txt')
    check_st, st_dset, stops_dset = check_stop_ids(st_data, stops_data)
    if check_st:
        print("  Removing invalid stop ids from stop_times.txt")
        unzip_to_file(archive, path0, path, stop_dataset=stops_dset, st_dataset=st_dset)
    st_data.close()
    stops_data.close()
    # Fix error that occurs rarely in stop_times.txt, no stop time for final stop
    archive = zipfile.ZipFile(path)
    stop_times_data = archive.open('stop_times.txt')
    check_st, st_dset = check_stop_times(stop_times_data)
    if check_st:
        print("  Interpolating stop times with no final stop time")
        unzip_to_file(archive, path0, path, stop_dataset=st_dset)
    stop_times_data.close()
    # Fix error that occurs rarely in transfers.txt, where values for transfer_type are missing
    archive = zipfile.ZipFile(path)
    if 'transfers.txt' in archive.namelist():  # transfers.txt is optional, and does not have to appear in all GTFS feeds
        transfer_data = archive.open('transfers.txt')
        check_tr, tr_dset = check_transfers(transfer_data)
        if check_tr:
            print("  Fixing transfer type field in transfers")
            unzip_to_file(archive, path0, path, transfer_dataset=tr_dset)
        transfer_data.close()
    # fix error that can occur in pathways.txt where OTP looks for required pathways_type buy file names pathways_mdoe
    archive = zipfile.ZipFile(path)
    if 'pathways.txt' in archive.namelist(): #pathways.txt is optional, and does not have to appear in all GTFS feeds
        pathways_data = archive.open('pathways.txt')
        paths = pd.read_csv(pathways_data)
        paths.columns = [x.strip() for x in paths.columns]
        if "path_type" not in paths.columns:
            unzip_to_file(archive, path0, path, pathways_dataset=paths)
        pathways_data.close()
    # Fix error that occurs rarely in agency.txt, required fields agency_name,agency_url,agency_timezone missing, extra characters in final line (removed automatically by pandas)
    archive = zipfile.ZipFile(path)
    agency_data = archive.open('agency.txt')
    check_ag, ag_dset = check_agency(agency_data)
    if check_ag:
        print("  Fixing missing fields in agency file")
        unzip_to_file(archive, path0, path, agency_dataset=ag_dset)
    agency_data.close()
    # Fix duplicates in trips.txt on trip_id, and fix error where route_id in trips.txt not in routes.txt
    archive = zipfile.ZipFile(path)
    trips_data = archive.open('trips.txt')
    routes_data = archive.open('routes.txt')
    check_tri, tri_dset, rou_dset = check_trips(trips_data, routes_data)
    if check_tri:
        print("  Fixing duplicates, route_ids in trips file")
        unzip_to_file(archive, path0, path, trips_dataset=tri_dset, routes_dataset=rou_dset)
    trips_data.close()
    routes_data.close()
    # Fix rare error where calendar_dates.txt date field takes a value that is not YYYYMMDD
    archive = zipfile.ZipFile(path)
    if 'calendar_dates.txt' in archive.namelist():  # calendar_dates.txt is optional, and does not have to appear in all GTFS feeds
        caldates_data = archive.open('calendar_dates.txt')
        check_cal, cal_dset = check_caldates(caldates_data)
        if check_cal:
            print("  Fixing calendar dates file")
            unzip_to_file(archive, path0, path, caldate_dataset=cal_dset)
        caldates_data.close()
    # Fix duplicate route ids in routes.txt
    archive = zipfile.ZipFile(path)
    routes_data = archive.open('routes.txt')
    check_rou, rou_dset = check_routes(routes_data)
    if check_rou:
        print("  Fixing duplicate route ids")
        unzip_to_file(archive, path0, path, routes_dataset=rou_dset)
    routes_data.close()
    return zipfile.ZipFile(path)


def fix_stop_time_file(dframe):
    # Description: Removes erroneous leading and trailing spaces from pandas file, which occurs very rarely
    # Input: Python open file
    # Output: pandas dataframe
    d_check_file = pd.read_csv(dframe)
    d_check_file.columns = [x.strip() for x in d_check_file.columns]
    for k in range(len(d_check_file.columns)):
        if str(d_check_file.dtypes[k]) == 'object':
            d_check_file[d_check_file.columns[k]] = [str(x).strip().replace('nan', '') for x in d_check_file[d_check_file.columns[k]]]
    return d_check_file


def check_stop_times(stop_file):
    # Description: For zipfiles that have stop_times.txt where the last stop has no time value, check if this is the case, and if so, return the dataset (efficiency)
    # Input: Python open file
    # Output: Boolean True (if there is an error) or False (if there is no error), and if True, a Pandas dataframe as a second return value, or None if False
    stimes = fix_stop_time_file(stop_file)
    stimes["arrival_time"].loc[stimes["arrival_time"] == ""] = np.nan
    stimes["departure_time"].loc[stimes["departure_time"] == ""] = np.nan
    stimes["arrival_time"].loc[(stimes["arrival_time"].isnull()) & (stimes["departure_time"].notnull())] = stimes["departure_time"].loc[(stimes["arrival_time"].isnull()) & (stimes["departure_time"].notnull())]
    stimes["departure_time"].loc[(stimes["departure_time"].isnull()) & (stimes["arrival_time"].notnull())] = stimes["arrival_time"].loc[(stimes["departure_time"].isnull()) & (stimes["arrival_time"].notnull())]
    last_vals = stimes.groupby('trip_id', as_index=False)['stop_sequence'].max()
    last_vals["last"] = 1
    stimes_max = stimes.merge(last_vals, how="left", on=["trip_id", "stop_sequence"])
    stimes_max["missing"] = 0
    stimes_max["missing"].loc[(stimes_max["last"] == 1) & (stimes_max["arrival_time"].isnull())] = 1
    if sum(stimes_max["missing"]) > 0:
        return True, stimes_max
    else:
        return False, None


def interp_stop_times(sdata, fname):
    # Description: For zipfiles that have stop_times.txt where the last stop has no time value, add interpolated guess
    # Input:
    #   - sdata: Pandas dataframe of stop_times.txt file with missings marked
    #   - fname: Full path of the file, for writing data out
    # Output: None, writes data out to file
    def_time = 5  # 5 minutes between stops if there's only one time
    def_time_amt = 8  # Default time is 8 am for first stop if all stop times are blank
    missing_ids = sdata["trip_id"].loc[sdata["missing"] == 1]
    sdata_missing = sdata.loc[sdata["trip_id"].isin(missing_ids)]
    for id in missing_ids:
        cur_id = sdata_missing.loc[sdata_missing["trip_id"] == id]
        exist_id = cur_id.loc[cur_id["arrival_time"].notnull()]
        max_cur = cur_id["stop_sequence"].values[-1]
        if len(exist_id.index) == 0:
            mval = str(dt.timedelta(hours=def_time_amt) + dt.timedelta(minutes=def_time * len(cur_id.index)))
        elif len(exist_id.index) == 1:
            where_n = exist_id["arrival_time"].values[0]
            where_max = exist_id["stop_sequence"].values[0]
            mval = str(pd.to_timedelta(where_n) + dt.timedelta(minutes=def_time * (len(cur_id.index) - int(where_max)))).split("days ")[-1]
        else:
            def_time_int = (pd.to_timedelta(exist_id["arrival_time"].values[-1]) - pd.to_timedelta(exist_id["arrival_time"].values[-2])).seconds/60
            t_distance = int(exist_id["stop_sequence"].values[-1] - exist_id["stop_sequence"].values[-2])
            def_time = def_time_int / t_distance
            nvals = len(cur_id.index) - int(exist_id["stop_sequence"].values[-1])
            mval = str(pd.to_timedelta(exist_id["arrival_time"].values[-1]) + dt.timedelta(minutes=def_time * nvals)).split("days ")[-1]
        sdata["arrival_time"].loc[(sdata["stop_sequence"] == max_cur) & (sdata["trip_id"] == id)] = mval
        sdata["departure_time"].loc[(sdata["stop_sequence"] == max_cur) & (sdata["trip_id"] == id)] = mval
    sdata.to_csv(fname, index=False)

def fix_pathways(paths):
    if "pathway_mode" in paths.columns:
        paths.rename(columns = {"pathway_mode": "pathway_type"}, inplace = True)
    
    if "pathway_type" in paths.columns:
        return True, paths
    else:
        return False, None

def check_transfers(trans_data):
    # Description: For transfer files that do not have values for transfer_type, add a default value of 0
    # Input: Python open file
    # Output: Boolean True (if there is an error) or False (if there is no error), and if True, a Pandas dataframe as a second return value, or None if False
    ttimes = pd.read_csv(trans_data)
    ttimes_missing = ttimes.loc[ttimes["transfer_type"].isnull()]
    if len(ttimes_missing.index) > 0:
        return True, ttimes
    else:
        return False, None


def check_agency(agen_data):
    # Description: For agency files that have required values missing, check for that
    # Input: Python open file
    # Output: Boolean True (if there is an error) or False (if there is no error), and if True, a Pandas dataframe as a second return value, or None if False
    atimes = pd.read_csv(agen_data)
    atimes.columns = [x.strip() for x in atimes.columns]
    req_fields = ["agency_name", "agency_url", "agency_timezone"]
    for req in req_fields:
        if len(atimes.loc[atimes[req].isnull()].index) > 0:
            return True, atimes
    all_lines = [len(str(x).split(',')) for x in agen_data]
    if len(set(all_lines)) > 1:
        return True, atimes
    else:
        return False, None


def check_trips(tri_data, rou_data):
    # Description: For trips.txt files with duplicates, remove duplicates, and fix missing route_ids
    # Input: Python open file
    # Output: Boolean True (if there is an error) or False (if there is no error), and if True, a Pandas dataframe as a second return value, or None if False
    tritimes = fix_stop_time_file(tri_data)
    route_data = fix_stop_time_file(rou_data)
    if len(tritimes.index) != len(tritimes.drop_duplicates('trip_id').index):
        return True, tritimes, route_data
    else:
        route_ids = route_data["route_id"].values.tolist()
        trilen = tritimes.loc[tritimes["route_id"].isin(route_ids)]
        if len(trilen.index) < len(tritimes.index):
            return True, tritimes, route_data
        else:
            return False, None, None


def check_caldates(caldate_data):
    # Description: For calendar_dates.txt files with invalid values, fix them
    # Input: Python open file
    # Output: Boolean True (if there is an error) or False (if there is no error), and if True, a Pandas dataframe as a second return value, or None if False
    caldata = fix_stop_time_file(caldate_data)
    caldata["date"] = caldata["date"].astype(int)
    if len(caldata.loc[caldata["date"] > 10000].index) < len(caldata.index):
        return True, caldata
    else:
        return False, None


def check_routes(routes_data):
    # Description: For routes.txt files with duplicate ids
    # Input: Python open file
    # Output: Boolean True (if there is an error) or False (if there is no error), and if True, a Pandas dataframe as a second return value, or None if False
    routesdata = fix_stop_time_file(routes_data)
    rlen = len(routesdata.index)
    routesdata = routesdata.drop_duplicates('route_id')
    if len(routesdata.index) < rlen:
        return True, routesdata
    else:
        return False, None


def check_stop_ids(s_data, stop_data):
    # Description: For stop_times.txt file with invalid stop ids
    # Input: Python open files (s_data = stops.txt, stop_data = stop_times.txt)
    # Output: Boolean True (if there is an error) or False (if there is no error), and if True, a Pandas dataframe as a second return value, or None if False
    stdata = fix_stop_time_file(s_data)
    stopdata = fix_stop_time_file(stop_data)
    all_stop_ids = stdata["stop_id"].unique().tolist()
    stopcheck = stopdata.loc[~stopdata["stop_id"].isin(all_stop_ids)]
    if len(stopcheck.index) > 0 and len(stopcheck.index) < len(stopdata.index):  # Ignores those where datatypes differ between stops.txt and stop_times.txt, could introduce a future error
        return True, stdata, stopdata
    else:
        return False, None, None


print()
print("Correcting GTFS Errors")
print()

gtfs_path = "data/gtfs-raw"
data = os.listdir(gtfs_path)
data = ['/'.join([gtfs_path, x]) for x in data]

for i in range(len(data)):
    print(data[i])
    archive = clean_zipfile(data[i])
    # Check if location_type is in stops.txt
    stop_data = archive.open('stops.txt')
    vars = stop_data.readline().decode('utf-8').replace('\n', '').split(',')
    if 'location_type' in vars:
        # Check to make sure all values of location_type in stop_times.txt == 0
        stop_times_data = archive.open('stop_times.txt')
        stop_df = pd.read_csv(stop_times_data)
        if 'location_type' in stop_df.columns:
            print()
            print("!!!Error that may cause build to fail!!!")
            print(data[i]["name"], stop_df["location_type"].unique())
            print("!!!Error that may cause build to fail!!!")
            print()

print()
print("Finished! Remember to sync the subdirectory to S3, and move the metadata file as well")
