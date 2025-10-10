#!/usr/bin/env python3
"""
overwrite_arcgis_item.py
--------------------------------
Overwrites a hosted feature layer or table on ArcGIS Online
with a local CSV or shapefile, using username + password login.

Usage:
  python overwrite_arcgis_item.py <username> <password> <item_id> <csv_path>

Example:
  python overwrite_arcgis_item.py my_user my_pass 821d4e5f0f66457497041824909fbebc output/RFC_GW_Forecast.csv
"""

import sys
import os
from arcgis.gis import GIS
from arcgis.features import FeatureLayerCollection

def main():
    if len(sys.argv) != 5:
        print("Usage: python overwrite_arcgis_item.py <username> <password> <item_id> <csv_path>")
        sys.exit(1)

    username = sys.argv[1]
    password = sys.argv[2]
    item_id = sys.argv[3]
    csv_path = sys.argv[4]
    
    if not os.path.exists(csv_path):
        print(f"❌ CSV file not found: {csv_path}")
        sys.exit(1)
    
    print("🔐 Logging into ArcGIS Online...")
    gis = GIS("https://www.arcgis.com", username, password)
    print(f"✅ Logged in as: {gis.users.me.username}")
    
    print(f"📦 Fetching item {item_id}...")
    item = gis.content.get(item_id)
    if not item:
        print(f"❌ Item not found: {item_id}")
        sys.exit(1)
    
    print(f"✅ Found item: {item.title} (type: {item.type})")
    
    if item.type.lower() not in ["feature layer", "feature service"]:
        print("⚠️ Item is not a Feature Layer/Service — overwrite may not apply.")
        sys.exit(1)
    
    print(f"⬆️ Uploading and overwriting data from: {csv_path}")
    flayer_collection = FeatureLayerCollection.fromitem(item)
    flayer_collection.manager.overwrite(csv_path)
    
    print("✅ Successfully overwritten hosted feature layer data!")

if __name__ == "__main__":
  main()
