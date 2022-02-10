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
Map.centerObject(region, 11); //17, 11
Map.addLayer(region, {color: '000000'}, 'PC_bound_refined', false);
print('Polygon area: ', region.area().divide(1000 * 1000));


// Read the LSP 
var new_bands = ['gri', 'giMD', 'gre', 'sei', 'seMD', 'see', 
         'griVI', 'giMDVI', 'greVI', 'seiVI', 'seMDVI', 'seeVI', 
         'minVI', 'maxVI', 'grprate', 'setrate', 'growlength', 'growarea'];



//Read the Land cover proportion. 
var LC_HLS = ee.Image("users/jianmin/LC_proportions_HLSscale_masked");
print(LC_HLS);
Map.addLayer(LC_HLS, {bands:'Trees', min:0, max:0.9, palette:rainbow_p}, 'HLS_Tree', false);
Map.addLayer(LC_HLS, {bands:'Shrubs', min:0, max:0.9, palette:rainbow_p}, 'HLS_Shrub', false);
var vege = LC_HLS.select('Trees').add(LC_HLS.select('Shrubs')).rename('Vegetation');
var tree = LC_HLS.select('Trees').divide(vege).rename('Trees');
Map.addLayer(vege, {bands:'Vegetation', min:0, max:1, palette:rainbow_p}, 'HLS_VFC', false);

var LSP_HLS = ee.Image("users/jianminwang1989/LSP_HLS_Ponil_Complex_2018").rename(new_bands);
//LSP_HLS = LSP_HLS.updateMask(vege.gt(0.05));
Map.addLayer(LSP_HLS, {bands: 'gri', min:80, max: 200, palette:rainbow_p}, 'SOS_HLS', false);
Map.addLayer(LSP_HLS, {bands: 'see', min:200, max: 360, palette:rainbow_p}, 'EOS_HLS', false);


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






//Read the Climatology
var winter = ee.ImageCollection('NASA/ORNL/DAYMET_V3')
                  .filter(ee.Filter.date('2017-12-01', '2018-2-28'))
                  .select(['dayl', 'prcp', 'tmax', 'tmin'])
                  .reduce(ee.Reducer.mean())
                  .clip(region);
//print('winter', winter);
Map.addLayer(winter, {bands:'tmax_mean', min: 5.8, max: 10.5, palette:rainbow_p}, 'winter tmax', false);
Map.addLayer(winter, {bands:'tmin_mean', min: -7.9, max: -7.5, palette:rainbow_p}, 'winter tmin', false);
//Map.addLayer(winter, {bands:'prcp_mean', min: 0.05, max: 0.42, palette:rainbow_p}, 'winter prep');
winter = winter.rename(['dayl_winter', 'prcp_winter', 'tmax_winter', 'tmin_winter']);

var spring = ee.ImageCollection('NASA/ORNL/DAYMET_V3')
                  .filter(ee.Filter.date('2018-3-01', '2018-5-31'))
                  .select(['dayl', 'prcp', 'tmax', 'tmin'])
                  .reduce(ee.Reducer.mean())
                  .clip(region);
Map.addLayer(spring, {bands:'tmax_mean', min: 13.6, max: 19.5, palette:rainbow_p}, 'spring tmax', false);
Map.addLayer(spring, {bands:'tmin_mean', min: -1.0, max: 0, palette:rainbow_p}, 'spring tmin', false);
//Map.addLayer(spring, {bands:'prcp_mean', min: 0.2, max: 0.5, palette:rainbow_p}, 'spring prep');
spring = spring.rename(['dayl_spring', 'prcp_spring', 'tmax_spring', 'tmin_spring']);

var summer = ee.ImageCollection('NASA/ORNL/DAYMET_V3')
                  .filter(ee.Filter.date('2018-06-01', '2018-08-31'))
                  .select(['dayl', 'prcp', 'tmax', 'tmin'])
                  .reduce(ee.Reducer.mean())
                  .clip(region);
Map.addLayer(summer, {bands:'tmax_mean', min: 23.7, max: 29.3, palette:rainbow_p}, 'summer tmax', false);
Map.addLayer(summer, {bands:'tmin_mean', min: 8.5, max: 10.5, palette:rainbow_p}, 'summer tmin', false);
//Map.addLayer(summer, {bands:'prcp_mean', min: 0.6, max: 1.3, palette:rainbow_p}, 'summer prep');
summer = summer.rename(['dayl_summer', 'prcp_summer', 'tmax_summer', 'tmin_summer']);

var autumn = ee.ImageCollection('NASA/ORNL/DAYMET_V3')
                  .filter(ee.Filter.date('2018-09-01', '2018-11-30'))
                  .select(['dayl', 'prcp', 'tmax', 'tmin'])
                  .reduce(ee.Reducer.mean())
                  .clip(region);
Map.addLayer(autumn, {bands:'tmax_mean', min: 12.5, max: 17.6, palette:rainbow_p}, 'autumn tmax', false);
Map.addLayer(autumn, {bands:'tmin_mean', min: 0.11, max: 0.87, palette:rainbow_p}, 'autumn tmin', false);
//Map.addLayer(autumn, {bands:'prcp_mean', min: 1.0, max: 1.6, palette:rainbow_p}, 'autumn prep');
autumn = autumn.rename(['dayl_autumn', 'prcp_autumn', 'tmax_autumn', 'tmin_autumn']);




var MTBS = ee.Image("users/jianmin/MTBS_Ponil_Complex");

var inputs = ee.Image.cat([
  LSP_HLS.select(['gri', 'see', 'minVI', 'maxVI'])
  , LC_HLS
  , elev
  , slope
  , aspect
  , winter.select(['prcp_winter', 'tmax_winter', 'tmin_winter'])
  , spring.select(['prcp_spring', 'tmax_spring', 'tmin_spring'])
  , summer.select(['prcp_summer', 'tmax_summer', 'tmin_summer'])
  , autumn.select(['prcp_autumn', 'tmax_autumn', 'tmin_autumn'])
  /*
  , clim12.select(['prcp_12', 'tmax_12', 'tmin_12'])
  , clim01.select(['prcp_01', 'tmax_01', 'tmin_01'])
  , clim02.select(['prcp_02', 'tmax_02', 'tmin_02'])
  , clim03.select(['prcp_03', 'tmax_03', 'tmin_03'])
  , clim04.select(['prcp_04', 'tmax_04', 'tmin_04'])
  , clim05.select(['prcp_05', 'tmax_05', 'tmin_05'])
  , clim06.select(['prcp_06', 'tmax_06', 'tmin_06'])
  , clim07.select(['prcp_07', 'tmax_07', 'tmin_07'])
  , , clim08.select(['prcp_08', 'tmax_08', 'tmin_08'])
  , clim09.select(['prcp_09', 'tmax_09', 'tmin_09'])
  , clim10.select(['prcp_10', 'tmax_10', 'tmin_10'])
  , clim11.select(['prcp_11', 'tmax_11', 'tmin_11'])*/
  , MTBS
]).float();
print(inputs);
Export.image.toDrive({
  image: inputs,//.select('Onset_Greenness_Increase1'),
  description: "LSP_model_HLS_2018", //_OGI1
  maxPixels: 1e11,
  region: region,
  crs: 'EPSG:32613',
  crsTransform:[30,0,484620,0, -30,4074570]
}); 
print(inputs);


/*//Build up all the layers
var inputs = ee.Image.cat([
  LSP_HLS.select('gri').float(),
  vege,
  tree,
  elev.float(),
  slope,
  aspect,
  winter,
  spring
]);

print(inputs);
 //build up the Random forest model
var inputnames=['B1','B2','B3','B4','B5','B6','B7'];
var RFtrained = ee.Classifier.randomForest({
  
})
.setOutputMode('REGRESSION')
.train({
  features:,
  classProperty:'',
  inputProperties:
});





// Overlay the points on the imagery to get training.
var training = image_pre.select(bands).sampleRegions({
  collection: mypolygons,
  properties: [label],
  tileScale: 16,
  scale: 1
});

print(training);
print('training', training.size());
// Train a CART classifier with default parameters.
//var classifier  = ee.Classifier.smileCart().train(training, label, bands);
var classifier  = ee.Classifier.libsvm().train(training, label, bands);
var classifier  = ee.Classifier.smileRandomForest(10,null,1,0.6).train(training, label, bands);
var classified = image_pre. //image_pre. image2
  select(bands).classify(classifier );
//Run the RF model 
*/
