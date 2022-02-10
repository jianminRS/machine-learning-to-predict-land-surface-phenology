var water = 
    /* color: #d63000 */
    /* shown: false */
    ee.FeatureCollection(
        [ee.Feature(
            ee.Geometry.Polygon(
                [[[-105.11283933428277, 36.63082830939734],
                  [-105.11210977522694, 36.63034185124155],
                  [-105.11186301199757, 36.630410730477536],
                  [-105.11173426596486, 36.63111673909694],
                  [-105.11198102919423, 36.63136642352385],
                  [-105.11290370909535, 36.631039250662035]]]),
            {
              "class": 5,
              "system:index": "0"
            }),
        ee.Feature(
            ee.Geometry.Polygon(
                [[[-105.11756538836323, 36.6333940033064],
                  [-105.11734276404138, 36.633142173036646],
                  [-105.11695920912587, 36.63289895159443],
                  [-105.11680900542103, 36.6327698071465],
                  [-105.11672317473256, 36.632894646783],
                  [-105.11650859801136, 36.633101277461016],
                  [-105.11634766547047, 36.633217506973914],
                  [-105.116261834782, 36.63331651716144],
                  [-105.11609017340504, 36.63334665067153],
                  [-105.11593460528218, 36.633428441568036],
                  [-105.11577367274128, 36.63351023237776],
                  [-105.11555909602009, 36.63354467058744],
                  [-105.11550008742176, 36.63366520420016],
                  [-105.11539279906117, 36.63378143286243],
                  [-105.11534988371693, 36.633863223297446],
                  [-105.11535256589862, 36.63402034662487],
                  [-105.11541962115132, 36.634190384169365],
                  [-105.11552154509388, 36.634384097188764],
                  [-105.11572539297902, 36.63456920028562],
                  [-105.1158487745937, 36.63462946631422],
                  [-105.11596142737233, 36.63463377102875],
                  [-105.11606871573292, 36.634620856884446],
                  [-105.11631011454426, 36.63459502858936],
                  [-105.11662125078999, 36.6343195262364],
                  [-105.11698603121602, 36.633966537407076],
                  [-105.1172542521175, 36.633785737624336],
                  [-105.11743127791249, 36.63362646127378],
                  [-105.11749565092884, 36.63349731804516]]]),
            {
              "class": 5,
              "system:index": "1"
            }),
        ee.Feature(
            ee.Geometry.Polygon(
                [[[-105.11984977913936, 36.63471382753234],
                  [-105.11947426987727, 36.63467938984518],
                  [-105.11930797291835, 36.634696608690696],
                  [-105.1193884391888, 36.634804226387885],
                  [-105.11942062569697, 36.6349333674261],
                  [-105.11971566868861, 36.63496780499975],
                  [-105.11989805890163, 36.63486879693406]]]),
            {
              "class": 5,
              "system:index": "2"
            }),
        ee.Feature(
            ee.Geometry.Polygon(
                [[[-105.12533239848806, 36.635884790027696],
                  [-105.12554161079122, 36.63538114497374],
                  [-105.1256274414797, 36.634881801328284],
                  [-105.12547187335683, 36.634632128292004],
                  [-105.12493543155385, 36.634610604716514],
                  [-105.12465111739827, 36.6352261766025],
                  [-105.12468866832448, 36.635385449646265]]]),
            {
              "class": 5,
              "system:index": "3"
            }),
        ee.Feature(
            ee.Geometry.Polygon(
                [[[-105.13997819769546, 36.72531331634832],
                  [-105.13978507864638, 36.72521872414566],
                  [-105.13987090933486, 36.72515422939522],
                  [-105.13977971422835, 36.72515422939522],
                  [-105.13969388353988, 36.72501234075359],
                  [-105.13954367983504, 36.72493494683856],
                  [-105.13942566263839, 36.72488765051877],
                  [-105.13932373869582, 36.72497794346764],
                  [-105.13934519636794, 36.72511123286483],
                  [-105.13943102705642, 36.725227323441636],
                  [-105.13953295099898, 36.72527031990701],
                  [-105.13970997679397, 36.725253121323746],
                  [-105.13991918909713, 36.72539070988204]]]),
            {
              "class": 5,
              "system:index": "4"
            })]),
    IC1003 = ee.ImageCollection("users/jianminwang1989/PlanetS_Ponil_Complex_201810"),
    IC0626 = ee.ImageCollection("users/jianminwang1989/PlanetS_Ponil_Complex_201806"),
    IC0513 = ee.ImageCollection("users/jianminwang1989/PlanetS_Ponil_Complex_201805"),
    IC0402 = ee.ImageCollection("users/jianminwang1989/PlanetS_Ponil_Complex_201804_02"),
    IC0418 = ee.ImageCollection("users/jianminwang1989/PlanetS_Ponil_Complex_201804"),
    LC3m = ee.Image("users/jianmin/landcover_3m_classification_tif");



//Palette for mapping
//var palettes = require('users/gena/packages:palettes');
//var rainbow_p = palettes.misc.tol_rainbow[7];
//print(rainbow_p);
var rainbow_p = ['#781C81', '#3F60AE', '#539EB6', '#6DB388', '#CAB843', '#E78532', '#D92120'];
var rainbow5 = ['#781C81', '#539EB6', '#6DB388', '#CAB843', '#D92120'];
print('rainbow5', rainbow5);


var imageCollection = ee.ImageCollection("users/jianminwang1989/NAIP_Ponil_Complex_2018");
//print(imageCollection);
//Map.addLayer(imageCollection, {max:255}, "RGB_Mosaic", false);
//Read the region
var region = ee.FeatureCollection("users/jianmin/Ponil_Complex_refined_region");
region = region.geometry();
Map.centerObject(region, 20); //17, 11
Map.addLayer(region, {color: '000000'}, 'PC_bound_refined', false);
print('Polygon area: ', region.area().divide(1000 * 1000));


var pre_process = function(img, b_ndvi, b_bright){
  var ndvi = img.normalizedDifference(b_ndvi).rename('NDVI')
    .multiply(10000).toInt16();
  var bright = img.expression(
    '(b1 + b2 + b3 ) / 3', {
      'b1': img.select(b_bright[0]), 'b2': img.select(b_bright[1]), 
      'b3': img.select(b_bright[2])}).rename('Bright');
  //var bright = img.select(['b1', 'b2', 'b3']).reduce(ee.Reducer.mean());
  var res = ndvi.addBands(bright);
  /*
  //Based on NDVI and Brightness band to calculate the variance as TEXTURE
  //Calculate the standard reducer.
  var rads = ee.List.sequence(3, 10, 3);
  var texture = function(img){
    return function(radius){
      var text = img.reduceNeighborhood({
        reducer: ee.Reducer.stdDev(),
        kernel: ee.Kernel.square(radius, 'meters')
      });
      return text;
    };
  };
  
  var text_ndvi = ee.ImageCollection(rads.map(texture(ndvi)));
  var ndvi_sd = text_ndvi.reduce(ee.Reducer.mean()).rename('SD_NDVI');
  res = res.addBands(ndvi_sd);

  var text_bright = ee.ImageCollection(rads.map(texture(bright)));
  var bright_sd = text_bright.reduce(ee.Reducer.mean()).rename('SD_Bright');
  res = res.addBands(bright_sd);
  */
  return res;
  
};





var image = imageCollection.mosaic().clip(region);
/*
Map.addLayer(image.reproject({
  crs: 'EPSG:32613',
  crsTransform:[3,0,492528,0,-3,4078758]}
  ), {max:255}, "RGB_reprojected", false
);
image = image.reproject({
            //crs: 'EPSG:32613',
            //crsTransform:[0.6,0,492528,0,-0.6,4078758]
            crs: 'EPSG:32613',
            crsTransform:[0.6,0,488670,0,-0.6,4060368]
          })
    .reduceResolution({
            reducer:ee.Reducer.mean(),
            bestEffort:true,
            maxPixels: 300
          }).reproject({
            crs: 'EPSG:32613',
            crsTransform:[3,0,492528,0,-3,4078758]
          });*/
image = image.addBands(pre_process(image, ['b4', 'b1'], ['b1', 'b2', 'b3']));
Map.addLayer(image, {max:255}, "RGB_NAIP", false);
//Map.addLayer(image, {bands: 'NDVI', min:0, max: 5000, palette:rainbow_p}, 'NDVI_NAIP', false);
//Map.addLayer(image, {bands: 'Bright', min:30, max: 190, palette:rainbow_p}, 'Bright_NAIP', false);
var image_pre = image;
//print(image_pre.bandNames());
//print(image_pre);
var image0402 = IC0402.mosaic().clip(region);
/*
          .reproject({
            crs: 'EPSG:32613',
            crsTransform:[3,0,492528,0,-3,4078758]
          });*/
//Map.addLayer(IC0402, {max:1500, bands:['b3', 'b2', 'b1']}, "RGB_P0402", false);
image0402 = image0402.addBands(pre_process(image0402, ['b4', 'b3'], ['b1', 'b2', 'b3']));
//Map.addLayer(image0402, {max:1500, bands:['b3', 'b2', 'b1']}, "RGB_P0402", false);
//Map.addLayer(image0402, {bands: 'NDVI', min:0, max: 5000, palette:rainbow_p}, 'NDVI_P0402', false);
//Map.addLayer(image0402, {bands: 'Bright', min:400, max: 1300, palette:rainbow_p}, 'Bright_P0402', false);
image_pre = image_pre.addBands(image0402);
var image0418 = IC0418.mosaic().clip(region);
image0418 = image0418.addBands(pre_process(image0418, ['b4', 'b3'], ['b1', 'b2', 'b3']));
//Map.addLayer(image0418, {max:1500, bands:['b3', 'b2', 'b1']}, "RGB_P0418", false);
//Map.addLayer(image0418, {bands: 'NDVI', min:0, max: 5000, palette:rainbow_p}, 'NDVI_P0418', false);
//Map.addLayer(image0418, {bands: 'Bright', min:400, max: 1300, palette:rainbow_p}, 'Bright_P0418', false);
image_pre = image_pre.addBands(image0418);
var image0513 = IC0513.mosaic().clip(region);
image0513 = image0513.addBands(pre_process(image0513, ['b4', 'b3'], ['b1', 'b2', 'b3']));
//Map.addLayer(image0513,{max:1500, bands:['b3', 'b2', 'b1']}, "RGB_P0513", false);
//Map.addLayer(image0513, {bands: 'NDVI', min:0, max: 5000, palette:rainbow_p}, 'NDVI_P0513', false);
//Map.addLayer(image0513, {bands: 'Bright', min:400, max: 1300, palette:rainbow_p}, 'Bright_P0513', false);
image_pre = image_pre.addBands(image0513);
//NOT COMPLETED //users/jianminwang1989/PlanetS_Ponil_Complex_201806/20180625_171551_0f36_3B_AnalyticMS_SR
var image0626 = IC0626
                .filter(ee.Filter.or(ee.Filter.stringEndsWith('system:index', '1012_3B_AnalyticMS_SR'), ee.Filter.stringEndsWith('system:index', '0f36_3B_AnalyticMS_SR')))
                //.filter(ee.Filter.neq('system:index', '20180625_171551_0f36_3B_AnalyticMS_SR'))
                .mosaic().clip(region); 
//print(IC0626.filter(ee.Filter.or(ee.Filter.stringEndsWith('system:index', '1012_3B_AnalyticMS_SR'), ee.Filter.stringEndsWith('system:index', '0f36_3B_AnalyticMS_SR'))));
image0626 = image0626.addBands(pre_process(image0626, ['b4', 'b3'], ['b1', 'b2', 'b3']));
//Map.addLayer(image0626, {max:1500, bands:['b3', 'b2', 'b1']}, "RGB_P0626", false);
//Map.addLayer(image0626, {bands: 'NDVI', min:0, max: 5000, palette:rainbow_p}, 'NDVI_P0626', false);
//Map.addLayer(image0626, {bands: 'Bright', min:400, max: 1300, palette:rainbow_p}, 'Bright_P0626', false);
image_pre = image_pre.addBands(image0626);

var image1003 = IC1003
                .filter(ee.Filter.stringEndsWith('system:index', '0f51_3B_AnalyticMS_SR'))
                //.filter(ee.Filter.List('system:index', ['20181003_171706_0f51_3B_AnalyticMS_SR', '20181003_171707_0f51_3B_AnalyticMS_SR', ...]))
                .mosaic().clip(region);
//print(IC1003.filter(ee.Filter.stringEndsWith('system:index', '0f51_3B_AnalyticMS_SR')));
image1003 = image1003.addBands(pre_process(image1003, ['b4', 'b3'], ['b1', 'b2', 'b3']));
//Map.addLayer(image1003, {max:1500, bands:['b3', 'b2', 'b1']}, "RGB_P1003", false);
//Map.addLayer(image1003, {bands: 'NDVI', min:0, max: 5000, palette:rainbow_p}, 'NDVI_P1003', false);
//Map.addLayer(image1003, {bands: 'Bright', min:0, max: 5000, palette:rainbow_p}, 'Bright_P1003', false);
image_pre = image_pre.addBands(image1003);
print(image_pre.bandNames());
print(image_pre);




//Plot on the Rodman Class
//var rodmanclass = ee.Image('users/jianminwang1989/Ponil_Complex_Rodman_2014').
//  clip(region);
//Map.addLayer(rodmanclass, {min:0, max:1}, 'Rodman');




// Clustering
//Select the training samples
var training = image_pre.sample({
  seed:0,
  region: region,
  scale: 3,
  //factor:0.01,
  tileScale: 16,
  numPixels: 20000
});
print('Here', training.name());
print(training.size());


// Instantiate the clusterer and train it.
var nclus = 50;//40
var clusterer = ee.Clusterer.wekaKMeans({
  nClusters:nclus,
  //init: 2,
  //canopies:true,
  //t1:-1.5,
  //t2:-1
}).train(training);
print('b', clusterer.name());
print(clusterer);

// Cluster the input using the trained clusterer.
var result = image_pre.cluster(clusterer);
print('b', result.bandNames());
//Display the clusters with random colors.
print('projection', IC0402.first().projection());
result = result.reproject({scale:3, crs:IC0402.first().projection()} ); //rodmanclass.projection()
print('scale', result.projection().nominalScale());
Map.addLayer(result.randomVisualizer(), {}, 'clusters'); //, false
print('result', result);


//throw('stop');
/*
var themax = result.reduceRegion({reducer:ee.Reducer.max(), geometry:region, scale:10, maxPixels:1e10, tileScale:16});
print(themax);
var themin = result.reduceRegion({reducer:ee.Reducer.min(), geometry:region, scale:10, maxPixels:1e10, tileScale:16});
print(themin);*/
//var oldclass = ee.List.sequence(0, nclus);
//print(oldclass);//6:3 or 2  9:3 or 2  
//var oldclass = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29];
//var newclass = [3, 0, 3, 3, 2, 1, 2, 2, 3, 3,  0,  3,  0,  0,  3,  0,  2,  0,  0,  3,  0,  0,  0,  0,  2,  0,  0,  2,  2,  2];
//This is for the classification with 0626 but not completed (without 0625)
//var oldclass = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39];
//var newclass = [3, 0, 3, 3, 2, 0, 3, 2, 3, 3,  0,  3,  3,  0,  3,  0,  2,  0,  2,  2,  0,  0,  0,  0,  2,  3,  0,  2,  2,  2,  0,  0,  2,  0,  3,  0,  3,  3,  3,  2];
//This is for the classification with 0625 and 0626 for 40 classes
//var oldclass = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39];
//var newclass = [1, 1, 1, 4, 4, 4, 4, 3, 4, 1,  3,  4,  4,  4,  3,  1,  3,  1,  1,  4,  1,  3,  1,  3,  3,  3,  3,  4,  1,  4,  4,  1,  1,  4,  1,  3,  3,  4,  4,  3];
//27: 3 or 4, 1: 1 or 4, 11: 3 or 4, 35: 3 or 4, 14 3 or 4, 8, 3 or 1, 18: 4 or 1, 8 4 or 1, 7 4 or 3


//This is for the classification with 0625 and 0626 for 50 classes
var oldclass = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49];
var newclass = [1, 4, 1, 4, 4, 4, 4, 3, 3, 1,  3,  4,  4,  4,  3,  1,  3,  1,  4,  4,  1,  3,  1,  3,  3,  3,  3,  4,  1,  4,  4,  1,  1,  4,  1,  4,  3,  4,  1,  1,  4,  1,  1,  4,  1,  4,  3,  3,  1,  1];
var newclass = [1, 4, 1, 4, 4, 4, 4, 3, 3, 1,  3,  4,  4,  1,  3,  1,  3,  1,  4,  4,  1,  3,  1,  3,  3,  3,  3,  4,  1,  4,  4,  1,  1,  4,  1,  4,  3,  4,  1,  3,  4,  1,  1,  4,  1,  4,  3,  3,  1,  4];
//21: 1 or 3, 4: 2 or 1 or 3, 38: 0 or 4, 
//13->1 35 ->3, 38->4, 39 ->3, 49->4

var res_remap = result.remap(oldclass, newclass, 5);
Map.addLayer(res_remap, {min:0, max:5, palette:['#FF0000', '#B5AFA2', '#000000', '#006633', '#B49C46', '#486DA2']}, 'remap', false);
print('res_remap', res_remap);
Map.addLayer(LC3m, {min:0, max:3, palette:['#B5AFA2', '#006633', '#B49C46', '#486DA2']}, 'LC3m', false);

//Class
//1: Bright soil  #B5AFA2   //1: Dark soil   #ffbc58
//2: Shadow #000000
//3: Trees  #006633
//4: Shrubs #B49C46 && Grass #0affd2
//5: Water  #486DA2
//0: Other


var watermask = water.reduceToImage({properties: ['class'], reducer: ee.Reducer.first()})
//  .reproject({crs:res_remap.projection()} )
  .remap([1, 2, 3, 4, 5], [1, 2, 3, 4, 5])
  .rename('remapped'); 
Map.addLayer(watermask, {}, 'water_mask', false);
print('watermask', watermask);
var res_comb = ee.ImageCollection.fromImages([res_remap, watermask]).max()
  .remap([1, 2, 3, 4, 5], [1, 2, 2, 3, 4], 0)
  .rename('class')
  .reproject({crs:res_remap.projection()} );
//throw('stop');
Map.addLayer(res_comb, {min:1, max:4, palette:['#B5AFA2', '#006633', '#B49C46', '#486DA2']}, 'remap_final', false);
print('res_comb', res_comb);
//var Buildup = result.remap(fromBuildup, toBuildup, 0, 'cluster').rename('Buildup');
//Class
//0: Bright soil  #B5AFA2   //1: Dark soil   #ffbc58
//1: Trees  #006633
//2: Shrubs #B49C46 && Grass #0affd2
//3: Water  #486DA2
//4: Other

Export.image.toDrive({
  image: res_comb,
  description: "landcover_3m",
  maxPixels: 1e11,
  region: region,
  //scale: 0.6,
  crs: 'EPSG:32613',
  crsTransform:[3,0,492528,0,-3,4078758] //x is Map Tie Point X and y is Map Tie Point Y
});


//Split each class to a single layer
var values = ee.List([0, 1, 2, 3]);
//var names = values.map(function(f) { return ee.Number(f).format("class %d") });
var names = ee.List(['Soil', 'Trees', 'Shrubs', 'Water']);
var split = res_comb.eq(ee.Image.constant(values).rename(names));



//Make statistics of the 3m planet classification map.
var new_bands = ['gri', 'giMD', 'gre', 'sei', 'seMD', 'see', 
         'griVI', 'giMDVI', 'greVI', 'seiVI', 'seMDVI', 'seeVI', 
         'minVI', 'maxVI', 'grprate', 'setrate', 'growlength', 'growarea'];
var LSP_HLS = ee.Image("users/jianminwang1989/LSP_HLS_Ponil_Complex_2018").rename(new_bands),
    LSP_VNP = ee.Image("users/jianminwang1989/LSP_VNP_Ponil_Complex_2018").rename(new_bands);
//print('LSP_HLS', LSP_HLS);
//print('LSP_VNP', LSP_VNP);
//Map.addLayer(LSP_HLS, {bands:'gri'}, 'LSP_HLS_gri');
//Map.addLayer(LSP_VNP, {bands:'gri'}, 'LSP_VNP_gri');

var LC_HLS = split.reduceResolution({
  reducer:ee.Reducer.mean(),
  maxPixels: 120
}).reproject({
  crs:LSP_HLS.projection()
});
print('LC_HLS', LC_HLS);
//var sum = LC_HLS.reduce(ee.Reducer.sum());
//LC_HLS = LC_HLS.divide(sum).multiply(100.0);
//print('LC_HLS2', LC_HLS);
Map.addLayer(LC_HLS, {bands:'Trees'}, 'LC_HLS', false);
Export.image.toAsset({
  image: LC_HLS,
  description: "LC_proportions_HLSscale",
  assetId: "LC_proportions_HLSscale",
  maxPixels: 1e11,
  region: region,
  //scale: 0.6,
  crs: 'EPSG:32613',
  crsTransform:[30,0,484620,0, -30,4074570] //x is Map Tie Point X and y is Map Tie Point Y
});

/*
//after the LC_HLS exported, Put in another code to do this. 
var LC_VNP = LC_HLS.reduceResolution({
  reducer:ee.Reducer.mean(),
  bestEffort:true,
  maxPixels: 26000
}).reproject({
  crs:LSP_VNP.projection()
});//.clip(region);
print('LC_VNP', LC_VNP);
//var sum = LC_VNP.reduce(ee.Reducer.sum());
//LC_VNP = LC_VNP.divide(sum).multiply(100.0);
//print('LC_VNP', LC_VNP);
Map.addLayer(LC_VNP, {bands:'Trees'}, 'LC_VNP');
Export.image.toAsset({
  image: LC_VNP,
  description: "LC_proportions_VNPscale",
  assetId: "LC_proportions_VNPscale",
  maxPixels: 1e4,
  region: region,
  //tileScale: 16,
  //scale: 480,
  crs: 'EPSG:32613',
  crsTransform:[480,0,484620,0, -480,4074570] //x is Map Tie Point X and y is Map Tie Point Y
});




//after the LC_HLS exported, Put in another code to do this. 
var LC_MOD = LC_HLS.reduceResolution({
  reducer:ee.Reducer.mean(),
  bestEffort:true,
  maxPixels: 26000
}).reproject({
  crs:'EPSG:32613', 
  crsTransform:[240,0,484620,0, -240,4074570] 
});//.clip(region);
print('LC_VNP', LC_MOD);
//print(LSP_MOD.projection())
//var sum = LC_VNP.reduce(ee.Reducer.sum());
//LC_VNP = LC_VNP.divide(sum).multiply(100.0);
//print('LC_VNP', LC_VNP);
Map.addLayer(LC_MOD, {bands:'Trees'}, 'LC_MOD');
Export.image.toAsset({
  image: LC_MOD,
  description: "LC_proportions_MODscale",
  assetId: "LC_proportions_MODscale",
  maxPixels: 1e6,
  region: region,
  //tileScale: 16,
  //scale: 480,
  crs: 'EPSG:32613',
  crsTransform:[240,0,484620,0, -240,4074570] //x is Map Tie Point X and y is Map Tie Point Y
});
*/
