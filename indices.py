import os
import pandas as pd
from functools import partial
from metpy.calc import k_index, total_totals_index, galvez_davison_index, lifted_index, mixed_parcel, parcel_profile, showalter_index
from metpy.units import units
from numpy import concatenate

# Setup
base_path = "E:/Datos"
file_suffix = "-84628.csv"

# 1. Import ----
indices = []

for year in range(1973, 2022):  # Note: range stops before 2022
    for month in range(1, 13):
        month_str = f"{month:02d}"  # Zero-padded month
        for day in range(1, 32):    # Will try all days up to 31
            day_str = f"{day:02d}"   # Zero-padded day
            for hour in ["00", "12"]:
                file_path = os.path.join(
                    base_path, 
                    str(year), 
                    f"{year}{month_str}{day_str}{hour}{file_suffix}"
                )
                
                try:
                    df = pd.read_csv(file_path)
                    
                    # Convert all numeric columns to float
                    for col in [0, 1, 2, 3, 7]:  
                        df.iloc[:, col] = pd.to_numeric(df.iloc[:, col], errors='coerce')

                    # Create arrays
                    p = df.iloc[:, 0].values.astype(float) * units.hPa       # Column 1: Pressure
                    h = df.iloc[:, 1].values.astype(float) * units.m         # Column 2: Height
                    T = df.iloc[:, 2].values.astype(float) * units.degC      # Column 3: Temperature
                    Td = df.iloc[:, 3].values.astype(float) * units.degC     # Column 4: Dewpoint
                    mr = df.iloc[:, 7].values.astype(float) * units('g/kg')  # Column 8: Mixing ratio

                    # Totals
                    tt = float(total_totals_index(p, T, Td).magnitude)
                    
                    # K 
                    ki = float(k_index(p, T, Td).magnitude)
                    
                    # Showalter
                    try:
                        si = float(showalter_index(p, T, Td).magnitude.item(0))
                    except ValueError:
                        si = 'NA'
                    # Lifted
                        
                    try:
                        # Calculate 500m mixed parcel
                        parcel_p, parcel_t, parcel_td = mixed_parcel(p, T, Td, depth=500 * units.m, height=h)
                        # Replace sounding temp/pressure in lowest 500m with mixed values
                        above = h > 500 * units.m
                        press = concatenate([[parcel_p], p[above]])
                        temp = concatenate([[parcel_t], T[above]])
                        # Calculate parcel profile from our new mixed parcel
                        mixed_prof = parcel_profile(press, parcel_t, parcel_td)
                        # Calculate lifted index using our mixed profile
                        li = float(lifted_index(press, temp, mixed_prof).magnitude.item(0))
                    except ValueError:
                        li = 'NA'

                    # Galvez-Davison
                    try:
                        gdi = float(galvez_davison_index(p, T, mr, p[0]).magnitude)
                    except ValueError:
                        gdi = 'NA'
                    # Add columns
                    indices.append({
                        'year': year,
                        'month': month_str,
                        'day': day_str,
                        'hour': hour,
                        'TT': tt,
                        'KI': ki,
                        'LI': li,
                        'SI': si,
                        'GDI': gdi
                    })
                
                except FileNotFoundError:
                    continue  # Skip if file doesn't exist

# Create DataFrame
indices_df = pd.DataFrame(indices)

# Export path
output_path = "E:/Datos/Resultados/indices_metpy.csv"  

# Export to CSV

indices_df.to_csv(output_path, index=False)
