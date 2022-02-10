//NOTE///
//        NOTE //
//This code can be used to calculate the preseason climates in 
//   in 3 three scenarios
//   1. period (DOY) is different in pixels and years 
//   2. period (DOY) is different in pixels but same in years. 
//   3. period (DOY) is the same in pixels and years. 
//   4. period (DOY) is the same in pixels but the different in years. 
//The only difference is the datelsp (2 places), where to use 
//         1. current year 
//         2. multi-year average (lspmeantp image)
//         3. multi-year and spatial average (lspmeantpsp: number)
//         4. spatial average (lspmeasp: number)

var rainbow_p = ['#781C81', '#3F60AE', '#539EB6', '#6DB388', '#CAB843', '#E78532', '#D92120'];
var rainbow5 = ['#781C81', '#539EB6', '#6DB388', '#CAB843', '#D92120'];
var rainbow8 = ["#FF00BFFF", "#8000FFFF", "#0040FFFF", "#00FFFFFF", "#00FF40FF",
   "#80FF00FF", "#FFBF00FF", "#FF0000FF"];
var terrain7 = ["#00A600FF", "#63C600FF", "#E6E600FF", "#E9BD3AFF", "#ECB176FF",
  "#EFC2B3FF", "#F2F2F2FF"];
print('rainbow5', rainbow5);


//Read the region
var region = ee.FeatureCollection("users/jianmin/Ponil_Complex_refined_region");
region = region.geometry();
Map.centerObject(region, 17); //17, 11
Map.addLayer(region, {color: '000000'}, 'PC_bound_refined', false);
print('Polygon area: ', region.area().divide(1000 * 1000));


//var years = [2001, 2003, 2004, 2005, 2006, 2007, 2008, 
//  2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018];
//years = [2016, 2018];
var years = [2018];
//var bands = ['2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008'
//  , '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018'];

// Read the LSP 
var new_bands = ['gri', 'giMD', 'gre', 'sei', 'seMD', 'see', 
         'griVI', 'giMDVI', 'greVI', 'seiVI', 'seMDVI', 'seeVI', 
         'minVI', 'maxVI', 'grprate', 'setrate', 'growlength', 'growarea'];
var LSP_HLS = ee.Image("users/jianminwang1989/LSP_HLS_Ponil_Complex_2018")
              .rename(new_bands);
Map.addLayer(LSP_HLS, {bands: 'gri', min:80, max: 200, palette:rainbow_p}, 'SOS_HLS');
Map.addLayer(LSP_HLS, {bands: 'see', min:200, max: 360, palette:rainbow_p}, 'EOS_HLS');


// Calculate the average sos and eos over individual pixels after masking 
var sos = LSP_HLS.select('gri');
var eos = LSP_HLS.select('see');
var lspmask = sos.gt(70).and(sos.lt(220))
              .and(eos.gt(200)).and(eos.lt(366));
var LSP = LSP_HLS.select(['gri', 'see', 'minVI', 'maxVI']).updateMask(lspmask);

var lspmeantp = LSP;
var lspmeantpsp = lspmeantp.reduceRegion({
                        reducer:ee.Reducer.mean(), 
                        geometry:region, 
                        crs: 'EPSG:32613',
                        crsTransform:[30,0,484620,0, -30,4074570],
                        maxPixels:1e10
});
print('lspmeantpsp', lspmeantpsp);
//print(ee.Number(lspmeantpsp.get('gri')).multiply(10));

//throw('stop');


//Read the Land cover proportion. 
var LC_HLS = ee.Image("users/jianmin/LC_proportions_HLSscale_masked");
print(LC_HLS);
Map.addLayer(LC_HLS, {bands:'Trees', min:0, max:1, palette:rainbow_p}, 'HLS_Tree', false);
Map.addLayer(LC_HLS, {bands:'Shrubs', min:0, max:1, palette:rainbow_p}, 'HLS_Shrub', false);
var vege = LC_HLS.select('Trees').add(LC_HLS.select('Shrubs')).rename('Vegetation');
var tree = LC_HLS.select('Trees').divide(vege).rename('Treesp');
Map.addLayer(vege, {bands:'Vegetation', min:0, max:1, palette:rainbow_p}, 'HLS_VFC', false);
Map.addLayer(tree, {bands:'Treesp', min:0, max:1, palette:rainbow_p}, 'HLS_TPV', false);

//Read the topography
var elev = ee.Image('USGS/SRTMGL1_003')
          .reproject({
            crs: 'EPSG:32613',
            crsTransform:[30,0,484620,0, -30,4074570]
          });
print(elev);
var slope = ee.Terrain.slope(elev);
var aspect = ee.Terrain.aspect(elev);
//aspect = aspect.add(22.5).divide(45).int8();  //round()
//aspect = aspect.remap([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], [0, 1, 2, 3, 4, 5, 6, 7, 0, 0]).rename('aspect');
//Map.addLayer(aspect, {bands:'aspect', min: 0, max: 7, palette:rainbow8}, 'aspect');
print(aspect);

elev = elev.clip(region);
Map.addLayer(elev, {min: 2070, max: 2810, palette:terrain7}, 'elev', false);
slope = slope.clip(region);
Map.addLayer(slope, {bands:'slope', min: 0, max: 21, palette:rainbow_p}, 'slope', false);
aspect = aspect.clip(region);
Map.addLayer(aspect, {bands:'aspect', min: 0, max: 360, palette:rainbow_p}, 'aspect', false);



var MTBS = ee.Image("users/jianmin/MTBS_Ponil_Complex")
          .reproject({
            crs: 'EPSG:32613',
            crsTransform:[30,0,484620,0, -30,4074570]
          });
Map.addLayer(MTBS, {min: 0, max: 6, palette:['#000000', '#006400', '7FFFD4', '#FFFF00', '#FF0000', '#FFFFF8', '#FFFFFF']}, 'mtbs', false);
Map.addLayer(ee.Image("users/jianmin/MTBS_Ponil_Complex"), {min: 0, max: 6, palette:['#000000', '#006400', '7FFFD4', '#FFFF00', '#FF0000', '#FFFFF8', '#FFFFFF']}, 'mtbs2', false);


//(EOS<366, EOS>200, SOS > 70, SOS<220)
var dayms = 86400000; //ee.Date('2018-01-02').millis().subtract(ee.Date('2018-01-01').millis());

//var steps = [30, 60, 90, 120, 150, 180];//ee.List.sequence(30, 180, 30); 
var steps = [];
for(var i=30;i<=180;i=i+30){steps.push(i);}
var LSP_driver_time_series = function(year){
  var byear = ee.Algorithms.String(year); 
  print(byear);
  var datestart = ee.Date.fromYMD(year-1, 12, 31).millis(); //the millis of first day of current year (actually last day of previous year)
  var sost1 = datestart.subtract(dayms*110); //70-180
  var sost2 = datestart.add(dayms*220); //220
  var eost1 = datestart.add(dayms*20); //200-180
  var eost2 = datestart.add(dayms*366); //220
  
  var sos = LSP_HLS.select('gri');
  var eos = LSP_HLS.select('see');
  var maxvi =  LSP_HLS.select('maxVI');
  var minvi =  LSP_HLS.select('minVI');
  
  var lspmask = sos.gt(70).and(sos.lt(220))
            .and(eos.gt(200)).and(eos.lt(366));
  var lspmeansp = ee.Dictionary({'gri':sos.updateMask(lspmask)
                           .reduceRegion({
                              reducer:ee.Reducer.mean(), 
                              geometry:region, 
                              crs: 'EPSG:32613',
                              crsTransform:[30,0,484620,0, -30,4074570],
                              maxPixels:1e10
                           }).get('gri'),
                  'see':eos.updateMask(lspmask)
                           .reduceRegion({
                              reducer:ee.Reducer.mean(), 
                              geometry:region, 
                              crs: 'EPSG:32613',
                              crsTransform:[30,0,484620,0, -30,4074570],
                              maxPixels:1e10
                           }).get('see')
  });
  print(lspmeansp);
  var cli_collection = ee.ImageCollection('NASA/ORNL/DAYMET_V3')
      .select(['srad', 'prcp', 'tmax', 'tmin']);
  
  //var datelsp = sos.updateMask(lspmask).multiply(dayms).add(datestart);  
  //var datelsp = lspmeantp.select('gri').multiply(dayms).add(datestart);
  var datelsp = ee.Number(lspmeantpsp.get('gri')).multiply(dayms).add(datestart); 
  //var datelsp = ee.Number(lspmeansp.get('gri')).multiply(dayms).add(datestart); 
  var pre = cli_collection.filter(ee.Filter.date(sost1, sost2))
              .map(function(image){
                var datemillis = ee.Image(ee.Number(image.get('system:time_start'))).toInt64()
                    .rename('datem');
                return image.addBands(datemillis)
                            .updateMask(lspmask)
                            .clip(region);
              });
  //print(steps);
  //print(presos);
  var presosmasked = steps.map(function(step){
    //print(step);
    var date_s = datelsp.subtract(dayms*step);
    var masked = pre.map(function(image){
      var tmean = image.select('tmax').add(image.select('tmin')).divide(2) //.subtract(0)
          .rename('tmean'); //tmean here is (tmax+tmin)
      var GDD = tmean.where(tmean.lt(0), 0).rename('GDD');
      var CD = tmean.lt(0).rename('CD');
      //var CDD = tmean.where(tmean.gt(0), 0).rename('CDD');
      return image
              //.addBands(tmean)
              .addBands(GDD).addBands(CD)
              .updateMask(image.select('datem').gt(date_s).and(image.select('datem').lte(datelsp)));
              //.updateMask(image.select('datem').lt(date_s).or(image.select('datem').gte(datelsp)));
    }); //image collection for different days
    //print(masked);
    var clinames = ['srad', 'prcp', 'GDD', 'CD', 'tmax', 'tmin'];
    var renames = clinames.map(function(name){
      return ee.String(name).cat(ee.String('SOS')).cat(ee.Algorithms.String(step));
    });
    var temp = masked.select(['tmax', 'tmin'])
                      .reduce(ee.Reducer.mean())
                      .rename(renames.slice(4, 6));
    return masked.select(['srad', 'prcp', 'GDD', 'CD'])
                .reduce(ee.Reducer.sum())
                .rename(renames.slice(0, 4))
                .addBands(temp); //image with bands of different climates
  }); //List of images
  //image collections for different steps or preseason periods
  //.toBands(); //image with bands of different climates and different steps
  //print('presosmasked', presosmasked);
  
  
  //var datelsp = see.select(byear).updateMask(lspmask).multiply(dayms).add(datestart);
  //var datelsp = lspmeantp.select('see').multiply(dayms).add(datestart); 
  var datelsp = ee.Number(lspmeantpsp.get('see')).multiply(dayms).add(datestart); 
  //var datelsp = ee.Number(lspmeansp.get('see')).multiply(dayms).add(datestart); 
  var pre = cli_collection.filter(ee.Filter.date(eost1, eost2))
              .map(function(image){
                var datemillis = ee.Image(ee.Number(image.get('system:time_start'))).toInt64()
                    .rename('datem');
                return image.addBands(datemillis)
                            .updateMask(lspmask)
                            .clip(region);
              });
  //print(steps);
  //print(pre);
  var preeosmasked = steps.map(function(step){
    //print(step);
    var date_s = datelsp.subtract(dayms*step);
    var masked = pre.map(function(image){
      var tmean = image.select('tmax').add(image.select('tmin')).divide(2) //.subtract(0)
          .rename('tmean'); //tmean here is (tmax+tmin)
      var GDD = tmean.where(tmean.lt(0), 0).rename('GDD');
      return image
              .addBands(GDD)
              .updateMask(image.select('datem').gt(date_s).and(image.select('datem').lte(datelsp)));
    }); //image collection for different days
    //print(masked);
    var clinames = ['srad', 'prcp', 'GDD', 'tmax', 'tmin'];
    var renames = clinames.map(function(name){
      return ee.String(name).cat(ee.String('EOS')).cat(ee.Algorithms.String(step));
    });
    var temp = masked.select(['tmax', 'tmin'])
                      .reduce(ee.Reducer.mean())
                      .rename(renames.slice(3, 5));
    return masked.select(['srad', 'prcp', 'GDD'])
                .reduce(ee.Reducer.sum())
                .rename(renames.slice(0, 3))
                .addBands(temp); //image with bands of different climates
  }); //List of images
  //image collections for different steps or preseason periods
  //.toBands(); //image with bands of different climates and different steps
  //print('preeosmasked', preeosmasked);             
  
  
  
  //Freeze and snow date
  var p1 = ee.Date.fromYMD(year-1, 7, 1).format('YYYY-MM-dd');
  var p2 = ee.Date.fromYMD(year, 7, 1).format('YYYY-MM-dd');
  var p3 = ee.Date.fromYMD(year, 7, 1).format('YYYY-MM-dd');
  var p4 = ee.Date.fromYMD(year+1, 7, 1).format('YYYY-MM-dd');
  
  var coldseason = ee.ImageCollection('NASA/ORNL/DAYMET_V3')
      .select('tmin')
      .filter(ee.Filter.date(p1, p2))
      .map(function(image){
          var datemillis = ee.Image(ee.Number(image.get('system:time_start'))).toInt64()
              .rename('datem');
          return image.addBands(datemillis)
                      .updateMask(image.select('tmin').lt(0))
                      .clip(region);
      });
  var ffreeze1 = coldseason.select('datem').min().subtract(datestart).divide(dayms).rename('FFD_SOS');
  var lfreeze1 = coldseason.select('datem').max().subtract(datestart).divide(dayms).rename('LFD_SOS');
  var dfreeze1 = lfreeze1.subtract(ffreeze1).add(1).rename('DF_SOS');
  coldseason = ee.ImageCollection('NASA/ORNL/DAYMET_V3')
      .select('tmin')
      .filter(ee.Filter.date(p3, p4))
      .map(function(image){
          var datemillis = ee.Image(ee.Number(image.get('system:time_start'))).toInt64()
              .rename('datem');
          return image.addBands(datemillis)
                      .updateMask(image.select('tmin').lt(0))
                      .clip(region);
      });
  var ffreeze2 = coldseason.select('datem').min().subtract(datestart).divide(dayms).rename('FFD_EOS');
  
  
  
  coldseason = ee.ImageCollection('MODIS/006/MOD10A1')
      .filter(ee.Filter.date(p1, p2))
      .map(function(image){
          var datemillis = ee.Image(ee.Number(image.get('system:time_start'))).toInt64()
              .rename('datem');
          var snowcover = image.select('NDSI_Snow_Cover');
          var snowqa = image.select('NDSI_Snow_Cover_Basic_QA');
          return image.addBands(datemillis)
                      .updateMask(snowcover.gt(0).and(snowcover.lt(101)).and(snowqa.lt(2)))
                      .clip(region);
      });
  var fsnow1 = coldseason.select('datem').min().subtract(datestart).divide(dayms).rename('FSF_SOS');
  var lsnow1 = coldseason.select('datem').max().subtract(datestart).divide(dayms).rename('LSD_SOS');
  var dsnow1 = lsnow1.subtract(fsnow1).add(1).rename('SCD_SOS');
  coldseason = ee.ImageCollection('MODIS/006/MOD10A1')
      .filter(ee.Filter.date(p3, p4))
      .map(function(image){
          var datemillis = ee.Image(ee.Number(image.get('system:time_start'))).toInt64()
              .rename('datem');
          var snowcover = image.select('NDSI_Snow_Cover');
          var snowqa = image.select('NDSI_Snow_Cover_Basic_QA');
          return image.addBands(datemillis)
                      .updateMask(snowcover.gt(0).and(snowcover.lt(101)).and(snowqa.lt(2)))
                      .clip(region);
      });
  var fsnow2 = coldseason.select('datem').min().subtract(datestart).divide(dayms).rename('FSF_EOS');
  
  //print(byear);
  var inputs = ee.Image.cat([
    sos.updateMask(lspmask).rename('gri'), eos.updateMask(lspmask).rename('see')
    , minvi.updateMask(lspmask).rename('minVI'), maxvi.updateMask(lspmask).rename('maxVI')
    , LC_HLS
    , elev
    , slope
    , aspect
    , MTBS.rename('BS')
    , ee.ImageCollection(presosmasked).toBands()
    , ee.ImageCollection(preeosmasked).toBands()
    , ffreeze1
    , lfreeze1
    , dfreeze1
    , ffreeze2
    , fsnow1
    , lsnow1
    , dsnow1
    , fsnow2
  ])
  .float();
  print(inputs.bandNames());
  //Map.addLayer(inputs.select('gri').updateMask(lspmask), {bands: 'gri', min:80, max: 200, palette:rainbow_p}, "SOS_"+byear.getInfo(), false);
  //Map.addLayer(inputs, {bands: 'see', min:200, max: 360, palette:rainbow_p}, "EOS_"+byear.getInfo(), false);
  //Map.addLayer(inputs, {bands: 'minVI', min:0, max: 3000, palette:rainbow_p}, "minVI_"+byear.getInfo(), false);
  //Map.addLayer(inputs, {bands: 'maxVI', min:0, max: 5000, palette:rainbow_p}, "maxVI_"+byear.getInfo(), false);
  
  
  //var taskname = ee.String(ee.String("LSP_model_MOD_").cat(byear));
  //print(taskname);
  Export.image.toDrive({
    image: inputs,//.select('Onset_Greenness_Increase1'),
    description: "LSP_preseasonmodel_HLS_"+byear.getInfo(), //_OGI1
    folder: 'LSP_HLS_preseason_snowfreeze_meantpsp', //_meantp  _meansptp  _meansp  _meanno
    maxPixels: 1e11,
    region: region,
    crs: 'EPSG:32613',
    crsTransform:[30,0,484620,0, -30,4074570]
  }); 
  //return(inputs);
  /*
  Export.image.toAsset({
    image: inputs,//.select('Onset_Greenness_Increase1'),
    description: "LSP_model_MOD500_"+byear.getInfo(), //_OGI1
    assetId: "LSP_model_MOD500_"+byear.getInfo(),
    maxPixels: 1e11,
    region: region,
    crs: 'EPSG:32613',
    crsTransform:[30,0,484620,0, -30,4074570]
  }); 
  */
  
};

var years = [2018];
var res = years.map(LSP_driver_time_series);
