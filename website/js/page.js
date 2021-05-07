var map = L.map('map', {
    center: [40.74505412378047, -73.94521497038218],
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
  var path_Brookyln1 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Bk1.geojson"
  var path_Brookyln2 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Bk2.geojson"
  var path_Bronx = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Bx.geojson"
  var path_Queens = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_Q.geojson"
  var path_Manhattan1 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M1.geojson"
  var path_Manhattan2 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/info_M2.geojson"
  //boundary and scenarios
  var path_boundary = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/boundary.geojson"
  var path_scenario1 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/scenario1.geojson"
  var path_scenario2 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/scenario2.geojson"
  var path_scenario3 = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/scenario3.geojson"
  //bikestations
  var path_Bikestations = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/bikestations.geojson"


  //bikelanes
  var path_bikelanes = "https://raw.githubusercontent.com/jiaxuanlyu/practium-nycdot-data/main/bikelanes.geojson"
  
  var info_Bronx;
  var info_Queens;
  var info_Brookyln;
  var info_Manhattan;

  var info_bikelanes;
  var boundary;
  var info_riderships;

  var featureGroup_riderships;
  var feature_bikestations;
  var featureGroup_bikelanes;
  var boundary_data;
  var info_bikestation;

  var info_scenario1;
  var info_scenario2;
  var info_scenario3;

  var feature_scenario1p;
  var feature_scenario1up;
  var feature_scenario2;
  var feature_scenario3p;
  var feature_scenario3up;

//default-hide the second legend
$(".legend2").hide()

//Events
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
      visBoundary(boundary_data)
      visRiderships(info_riderships)
      $(".legend").show()
      $(".legend2").hide()
      console.log("test")
  } else {
    map.removeLayer(featureGroup_riderships) && map.removeLayer(boundary)
    $(".legend").hide()
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

//scenarios
$("#scenario1-protected").change(function(){
  if(this.checked){
    visScenario1_protected(info_scenario1)
    $(".legend").hide()
    $(".legend2").show()
    checkCondition("#bikelanes")
    checkCondition("#bike-trips")
    checkCondition("#bike-stations")
    map.fitBounds(feature_scenario1p.getBounds());
  } else {
    map.removeLayer(feature_scenario1p)
    $(".legend2").hide()
  }
})

$('#scenario1-unprotected').change(function(){
  if(this.checked){
    visScenario1_Unprotected(info_scenario1)
    $(".legend").hide()
    $(".legend2").show()
    checkCondition("#bikelanes")
    checkCondition("#bike-trips")
    checkCondition("#bike-stations")
    map.fitBounds(feature_scenario1up.getBounds());
  } else {
    map.removeLayer(feature_scenario1up)
    $(".legend2").hide()
  }
})

$('#scenario2-protected').change(function(){
  if(this.checked){
    visScenario2(info_scenario2)
    $(".legend").hide()
    $(".legend2").show()
    checkCondition("#bikelanes")
    checkCondition("#bike-trips")
    checkCondition("#bike-stations")
    map.fitBounds(feature_scenario2.getBounds());
  } else {
    map.removeLayer(feature_scenario2)
    $(".legend2").hide()
  }
})

$('#scenario3-protected').change(function(){
  if(this.checked){
    visScenario3_protected(info_scenario3)
    $(".legend").hide()
    $(".legend2").show()
    checkCondition("#bikelanes")
    checkCondition("#bike-trips")
    checkCondition("#bike-stations")
    map.fitBounds(feature_scenario3p.getBounds());
  } else {
    map.removeLayer(feature_scenario3p)
    $(".legend2").hide()
  }
})

$('#scenario3-unprotected').change(function(){
  if(this.checked){
    visScenario3_Unprotected(info_scenario3)
    $(".legend").hide()
    $(".legend2").show()
    checkCondition("#bikelanes")
    checkCondition("#bike-trips")
    checkCondition("#bike-stations")
    map.fitBounds(feature_scenario3up.getBounds());
  } else {
    map.removeLayer(feature_scenario3up)
    $(".legend2").hide()
  }
})






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
    color: "#009AC9"
  }
}

//Styles

//get color
var getColor =  function(feature) {
  return feature.properties.Count == 0 ? "#6FA5C9":
         feature.properties.Count < 10 ? "#7DD8BD":
         feature.properties.Count < 30 ? "#E6F598":
         feature.properties.Count < 70 ? "#FFFFBF":
         feature.properties.Count < 130 ? "#FEE08B":
         feature.properties.Count < 250 ? "#FDAE61":
         feature.properties.Count < 350 ? "#F46D43":
         feature.properties.Count >= 350 ? "#D53E4F":
         '#4d004b';
}

var getColorDiff = function(feature) {
  return feature.properties.diff_protect < -50 ? "#1E6091":
         feature.properties.diff_protect < -10 ? "#2897CE":
         feature.properties.diff_protect < 0 ? "#85D3EB":
         feature.properties.diff_protect < 5 ? "#D9ED92":
         feature.properties.diff_protect < 10 ? "#99D98C":
         feature.properties.diff_protect < 30 ? "#52B69A":
         feature.properties.diff_protect < 50 ? "#FFBA08":
         feature.properties.diff_protect < 100 ? "#FAA307":
         feature.properties.diff_protect < 200 ? "#E85D04":
         feature.properties.diff_protect < 300 ? "#DC2F02":
         feature.properties.diff_protect < 500 ? "#9D0208":
         "#370617"
}

var getColorDiff2 = function(feature) {
  return feature.properties.diff_unprotect < -50 ? "#1E6091":
         feature.properties.diff_unprotect < -10 ? "#2897CE":
         feature.properties.diff_unprotect < 0 ? "#85D3EB":
         feature.properties.diff_unprotect < 5 ? "#D9ED92":
         feature.properties.diff_unprotect < 10 ? "#99D98C":
         feature.properties.diff_unprotect < 30 ? "#52B69A":
         feature.properties.diff_unprotect < 50 ? "#FFBA08":
         feature.properties.diff_unprotect < 100 ? "#FAA307":
         feature.properties.diff_unprotect < 200 ? "#E85D04":
         feature.properties.diff_unprotect < 300 ? "#DC2F02":
         feature.properties.diff_unprotect < 500 ? "#9D0208":
         "#370617"
}


//get Styles
function myStyle(feature) {
  return {
    fillColor: getColor(feature),
    color: getColor(feature),
    weight: 2,
    opacity: 0.73,
    fillOpacity: 0.7
  } ;
}

function diffStyle(feature) {
  return {
    fillColor: getColorDiff(feature),
    color: getColorDiff(feature),
    fillOpacity: 0.9
  }
}

function diffStyle2(feature) {
  return {
    fillColor: getColorDiff2(feature),
    color: getColorDiff2(feature),
    fillOpacity: 0.9
  }
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
  featureGroup_bikelanes = L.geoJson(data, {style: {color: '#2B94A9', opacity: 0.55}}).addTo(map);
}

//vis Riderships
var visRiderships = function(data) {
  featureGroup_riderships = L.geoJson(data, {style: myStyle}).bindPopup(function (layer) {
    return `Trip Count: ${layer.feature.properties.Count}`;
}).addTo(map);
}

//vis boundary
var visBoundary = function(data) {
  boundary = L.geoJson(data, {style: {color:"#83BFD1", opacity: 0.5}}).addTo(map)
}

//vis scenario1
var visScenario1_protected = function(data) {
  feature_scenario1p = L.geoJson(data, {style: diffStyle}).bindPopup(function (layer) {
    return `Actual Trips: ${layer.feature.properties.Count}, <br>
    Predicted Bike Trips: ${(layer.feature.properties.pre_protect).toFixed(2)}`;
}).addTo(map)
}

var visScenario1_Unprotected = function(data) {
  feature_scenario1up = L.geoJson(data, {style: diffStyle2}).bindPopup(function (layer) {
    return `Actual Trips: ${layer.feature.properties.Count}, <br>
    Predicted Bike Trips: ${(layer.feature.properties.pre_unprotect).toFixed(2)}`;
}).addTo(map)
}

//vis scenario2
var visScenario2 = function(data) {
  feature_scenario2 = L.geoJson(data, {style: diffStyle}).bindPopup(function (layer) {
    return `Actual Trips: ${layer.feature.properties.Count}, <br>
    Predicted Bike Trips: ${(layer.feature.properties.pre_protect).toFixed(2)}`;
}).addTo(map)
}

//vis scenario3

//Filter out 0s in Scenario3
var filterScenario3 = function(feature) {
  return feature.properties.pre_unprotect != 0; 
};

var visScenario3_protected = function(data) {
  feature_scenario3p = L.geoJson(data, {style: diffStyle}).bindPopup(function (layer) {
    return `Actual Trips: ${layer.feature.properties.Count}, <br>
    Predicted Bike Trips: ${(layer.feature.properties.pre_protect).toFixed(2)}`;
}).addTo(map)
}

var visScenario3_Unprotected = function(data) {
  feature_scenario3up = L.geoJson(data, {style: diffStyle2, filter:filterScenario3}).bindPopup(function (layer) {
    return `Actual Trips: ${layer.feature.properties.Count}, <br>
    Predicted Bike Trips: ${(layer.feature.properties.pre_unprotect).toFixed(2)}`;
}).addTo(map)
}


//Check conditions
//reference:https://stackoverflow.com/questions/901712/how-do-i-check-whether-a-checkbox-is-checked-in-jquery
var checkCondition = function(selector) {
  if($(selector).is(':checked')) {
    console.log("checked");  // checked
    $(selector).prop("checked", false).change()
  }
else {
  console.log("unchecked");  // unchecked
}
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




  $.when($.ajax(path_Brookyln1), $.ajax(path_Brookyln2), $.ajax(path_Bronx), $.ajax(path_Queens),
  $.ajax(path_Manhattan1), $.ajax(path_Manhattan2), $.ajax(path_boundary))
  .then(function(res1, res2, res3, res4, res5, res6, res7){
    Data1 = JSON.parse(res1[0])
    Data2 = JSON.parse(res2[0])
    Data3 = JSON.parse(res3[0])
    Data4 = JSON.parse(res4[0])
    Data5 = JSON.parse(res5[0])
    Data6 = JSON.parse(res6[0])
    info_riderships = concatGeoJSON6(Data1, Data2, Data3, Data4, Data5, Data6)

    boundary_data = JSON.parse(res7[0])

    $(".legend2").hide()
    visBoundary(boundary_data)
    visRiderships(info_riderships)
    

  })

  


  //bike stations
  $(document).ready(function() {
    $.ajax(path_Bikestations).done(function(data) {
      info_bikestation = JSON.parse(data)
    })
   

    $.ajax(path_bikelanes).done(function(data) {
      info_bikelanes = JSON.parse(data)
    })

    $.ajax(path_scenario1).done(function(data) {
      info_scenario1 = JSON.parse(data)
    })

    $.ajax(path_scenario2).done(function(data) {
      info_scenario2 = JSON.parse(data)
    })

    $.ajax(path_scenario3).done(function(data) {
      info_scenario3 = JSON.parse(data)
    })



  })



  


 