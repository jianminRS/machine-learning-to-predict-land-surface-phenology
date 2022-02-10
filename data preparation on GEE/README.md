Both codes were used to collect the data for the machine learning models at 30 m and 500 m, respectively.
The codes are also available on my GEE repository 
https://code.earthengine.google.com/add91c63aa11d9f4328bba4e4eae4b31?noload=true
https://code.earthengine.google.com/add91c63aa11d9f4328bba4e4eae4b31?noload=true
The data collected include: 
1. land cover composition 
    generated from NAIP and PlanetScope data on GEE; vegetation fraction cover - VFC, proportion of tree cover t oall vegetation cover - PTV
2. Preseason climate  
    generated from Daymet data; including Tmax, Tmin, Prcp, ShortWave Radiation, Growing degree days - GDD, Chilling days
4. Specific weather events
    generated from MODIS and Dayment; including First freeze date, last freeze date, duration of freeze, first snow date, last snow date, duration of snow. 
5. Topography
    generated from SRTM; including elevation, slope, northness, eastness
6. phenological events
    generated from MODIS/HLS; SOS
7. independent variable:
    generated from MODIS/HLS: SOS and EOS
