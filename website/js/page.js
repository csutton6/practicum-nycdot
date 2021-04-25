var map = L.map('map', {
    center: [40.77505412378047, -73.94521497038218],
    zoom: 12
  });
  
  var Stamen_TonerLite = L.tileLayer('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png', {
    attribution: 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>',
    subdomains: 'abcd',
    minZoom: 0,
    maxZoom: 20,
    ext: 'png'
  }).addTo(map);


  //data

  //Bornx
  var path_Bronx1 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Bronx1.geojson"
  var path_Bronx2 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Bronx2.geojson"
  var path_Bronx3 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Bronx3.geojson"
  var path_Bronx4 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Bronx4.geojson"
  var path_Bronx5 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Bronx5.geojson"
  var path_Bronx6 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Bronx6.geojson"


  //Queens
  var path_Queens1 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q1.geojson"
  var path_Queens2 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q2.geojson"
  var path_Queens3 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q3.geojson"
  var path_Queens4 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q4.geojson"
  var path_Queens5 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q5.geojson"
  var path_Queens6 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q6.geojson"
  var path_Queens7 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q7.geojson"
  var path_Queens8 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q8.geojson"
  var path_Queens9 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q9.geojson"
  var path_Queens10 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q10.geojson"
  var path_Queens11 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q11.geojson"
  var path_Queens12 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q12_1.geojson"
  var path_Queens13 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q13b.geojson"
  var path_Queens14 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q14.geojson"

  //Manhattan
  var path_Manhattan1 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M1.geojson"
  var path_Manhattan2 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M2.geojson"
  var path_Manhattan3 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M3.geojson"
  var path_Manhattan4 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M4.geojson"
  var path_Manhattan5 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M5.geojson"
  var path_Manhattan6 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M6.geojson"
  var path_Manhattan7 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M7.geojson"
  var path_Manhattan8 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M8.geojson"
  var path_Manhattan9 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M9.geojson"
  var path_Manhattan10 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M10.geojson"

  //bikestations
  var path_Bikestations = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/bikestations.geojson"


  //bikelanes
  var path_BikelanesBxQ = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/bikelanes_BxQ.geojson"
  var path_BikelanesM = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/bikelanes_M.geojson"
  var path_BikelanesBr = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/bikelanes_Br.geojson"

  var info_Bronx;
  var info_Queens;
  var info_Brookyln;
  var info_Manhattan;

  var info_bikelanes;

  var featureGroup_Bronx;
  var featureGroup_Queens;
  var feature_bikestations;
  var featureGroup_Manhattan;
  var featureGroup_bikelanes;

//check event
$("#bike-stations").change(function() {
  if(this.checked) {
      console.log("test")
      VisBikeStations(info_bikestation)
  } else {
    map.removeLayer(feature_bikestations)
  }
});

$("#bike-trips").change(function() {
  if(this.checked) {
      console.log("test")
  } else {
    return true
    //map.removeLayer(featureGroup_Bronx) && map.removeLayer(featureGroup_Queens)
  }
});

$("#bikelanes").change(function() {
  if(this.checked) {
      console.log("test")
      visBikeLanes(info_bikelanes)
  } else {
    map.removeLayer(featureGroup_bikelanes)
  }
});



//click event
var eachFeatureFunction = function(layer) {
  layer.on('click', function (event) {
    map.fitBounds(event.target.getBounds());
  })
}

//change size
var changeSize = function(feature) {
  return {
    radius: 3,
    color: "purple"
  }
}

//get color
var getColor =  function(feature) {
  return feature.properties.Count == 0 ? "rgba(6, 132, 141, 0.09)":
         feature.properties.Count < 2 ? "#fef0d9":
         feature.properties.Count < 5 ? "#fdd49e":
         feature.properties.Count < 10 ? "#fdbb84":
         feature.properties.Count < 20 ? "#fc8d59":
         feature.properties.Count < 50 ? "#ef6548":
         feature.properties.Count < 100 ? "#d7301f":
         feature.properties.Count > 100 ? "#990000":
         '#4d004b';
}



//Styles
function myStyle(feature) {
  return {
    fillColor: getColor(feature),
    color: getColor(feature),
    weight: 2,
    opacity: 1,
    fillOpacity: 0.8
  } ;
}


//visStations
var VisBikeStations = function(data){
  feature_bikestations = L.geoJson(data, {
    pointToLayer: function (feature, latlng) {
        return L.circleMarker(latlng, changeSize(feature))
    }
  }).addTo(map)
}

//visBikelanes
var visBikeLanes = function(data) {
  featureGroup_bikelanes = L.geoJson(data, {style: {color: '#B195D5', opacity: 0.7}}).addTo(map);
}



//conbine GeoJson in Bronx
function concatGeoJSON3(g1, g2, g3){
  return { 
      "type" : "FeatureCollection",
      "features": g1.features.concat(g2.features, g3.features)
  }
}


function concatGeoJSON6(g1, g2, g3, g4, g5, g6){
    return { 
        "type" : "FeatureCollection",
        "features": g1.features.concat(g2.features, g3.features, g4.features, g5.features, g6.features)
    }
}

function concatGeoJSON14(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14){
  return { 
      "type" : "FeatureCollection",
      "features": g1.features.concat(g2.features, g3.features, g4.features, g5.features, g6.features,
        g7.features, g8.features, g9.features, g10.features, g11.features, g12.features,
        g13.features, g14.features)
  }
}

function concatGeoJSON10(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10){
  return { 
      "type" : "FeatureCollection",
      "features": g1.features.concat(g2.features, g3.features, g4.features, g5.features, g6.features,
        g7.features, g8.features, g9.features, g10.features)
  }
}





  $.when($.ajax(path_Bronx1), $.ajax(path_Bronx2), $.ajax(path_Bronx3), $.ajax(path_Bronx4),
  $.ajax(path_Bronx5), $.ajax(path_Bronx6))
  .then(function(res_br1, res_br2, res_br3, res_br4, res_br5, res_br6){
    Bronx_Data1 = JSON.parse(res_br1[0])
    Bronx_Data2 = JSON.parse(res_br2[0])
    Bronx_Data3 = JSON.parse(res_br3[0])
    Bronx_Data4 = JSON.parse(res_br4[0])
    Bronx_Data5 = JSON.parse(res_br5[0])
    Bronx_Data6 = JSON.parse(res_br6[0])
    info_Bronx = concatGeoJSON6(Bronx_Data1, Bronx_Data2, Bronx_Data3, Bronx_Data4, Bronx_Data5, Bronx_Data6)
    

    featureGroup_Bronx = L.geoJson(info_Bronx, {style: myStyle}).addTo(map);

  })

  $.when($.ajax(path_Queens1), $.ajax(path_Queens2), $.ajax(path_Queens3), $.ajax(path_Queens4),
  $.ajax(path_Queens5), $.ajax(path_Queens6), $.ajax(path_Queens7), $.ajax(path_Queens8),
  $.ajax(path_Queens9), $.ajax(path_Queens10), $.ajax(path_Queens11), $.ajax(path_Queens12),
  $.ajax(path_Queens13), $.ajax(path_Queens14))
  .then(function(res_q1, res_q2, res_q3, res_q4, res_q5, res_q6, res_q7, res_q8, res_q9,
    res_q10, res_q11, res_q12, res_q13, res_q14){
    Queens_Data1 = JSON.parse(res_q1[0])
    Queens_Data2 = JSON.parse(res_q2[0])
    Queens_Data3 = JSON.parse(res_q3[0])
    Queens_Data4 = JSON.parse(res_q4[0])
    Queens_Data5 = JSON.parse(res_q5[0])
    Queens_Data6 = JSON.parse(res_q6[0])
    Queens_Data7 = JSON.parse(res_q7[0])
    Queens_Data8 = JSON.parse(res_q8[0])
    Queens_Data9 = JSON.parse(res_q9[0])
    Queens_Data10 = JSON.parse(res_q10[0])
    Queens_Data11 = JSON.parse(res_q11[0])
    Queens_Data12 = JSON.parse(res_q12[0])
    Queens_Data13 = JSON.parse(res_q13[0])
    Queens_Data14 = JSON.parse(res_q14[0])
    info_Queens = concatGeoJSON14(Queens_Data1, Queens_Data2, Queens_Data3, Queens_Data4, Queens_Data5, 
      Queens_Data6, Queens_Data7, Queens_Data8, Queens_Data9, Queens_Data10, Queens_Data11,
      Queens_Data12, Queens_Data13, Queens_Data14)
    

    featureGroup_Queens = L.geoJson(info_Queens, {style: myStyle}).addTo(map);

  })


  $.when($.ajax(path_Manhattan1), $.ajax(path_Manhattan2), $.ajax(path_Manhattan3), $.ajax(path_Manhattan4),
  $.ajax(path_Manhattan5), $.ajax(path_Manhattan6), $.ajax(path_Manhattan7), $.ajax(path_Manhattan8),
  $.ajax(path_Manhattan9), $.ajax(path_Manhattan10))
  .then(function(res_m1, res_m2, res_m3, res_m4, res_m5, res_m6, res_m7, res_m8, res_m9, res_m10){
    Manhattan_Data1 = JSON.parse(res_m1[0])
    Manhattan_Data2 = JSON.parse(res_m2[0])
    Manhattan_Data3 = JSON.parse(res_m3[0])
    Manhattan_Data4 = JSON.parse(res_m4[0])
    Manhattan_Data5 = JSON.parse(res_m5[0])
    Manhattan_Data6 = JSON.parse(res_m6[0])
    Manhattan_Data7 = JSON.parse(res_m7[0])
    Manhattan_Data8 = JSON.parse(res_m8[0])
    Manhattan_Data9 = JSON.parse(res_m9[0])
    Manhattan_Data10 = JSON.parse(res_m10[0])
    info_Manhattan = concatGeoJSON10(Manhattan_Data1, Manhattan_Data2, 
      Manhattan_Data3, Manhattan_Data4, Manhattan_Data5, Manhattan_Data6,
      Manhattan_Data7, Manhattan_Data8, Manhattan_Data9, Manhattan_Data10)
    
    
    featureGroup_Manhattan = L.geoJson(info_Manhattan, {style: myStyle}).addTo(map);

  })



  //bike stations
  $(document).ready(function() {
    $.ajax(path_Bikestations).done(function(data) {
      info_bikestation = JSON.parse(data)
      
    })
  })


//bikelanes
$.when($.ajax(path_BikelanesBxQ), $.ajax(path_BikelanesM), $.ajax(path_BikelanesBr))
.then(function(res_b1, res_b2, res_b3){
  bikelane_Data1 = JSON.parse(res_b1[0])
  bikelane_Data2 = JSON.parse(res_b2[0])
  bikelane_Data3 = JSON.parse(res_b3[0])
 
  info_bikelanes = concatGeoJSON3(bikelane_Data1, bikelane_Data2, bikelane_Data3)

})
  


 