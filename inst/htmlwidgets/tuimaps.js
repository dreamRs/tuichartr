HTMLWidgets.widget({

  name: 'tuimaps',

  type: 'output',

  factory: function(el, width, height) {

    var tuiChart = tui.chart;
    var widget, options = {}, data = {};
    var bbox, geojson, converter, svgStrings, geoptions, mapData, geodata;

    return {

      renderValue: function(x) {

        bbox = x.bbox, geojson = x.geojson, geodata = x.geodata;

        //console.log(height);
        //console.log(bbox);
        geoptions = {
          explode: false,
          viewportSize: {width: width, height: height},
          //mapExtent: {left: -180, bottom: -90, right: 180, top: 90},
          //mapExtent: {left: -1962011, bottom: -4139334, right: 5692196, top: 4488004},
          fitTo: 'height',
          mapExtent: bbox,
          output: 'path'
        };
        converter = geojson2svg(geoptions);
        svgStrings = converter.convert(geojson, geoptions);

        mapData = [];
        for (var i = 0; i < svgStrings.length; i++) {
          mapData.push({
            code: geodata.code[i],
            name: geodata.name[i],
            path: svgStrings[i],
            labelCoordinate: {x: 0.6, y: 0.7}
          });
        }
        //console.log(mapData);

        tuiChart.registerMap('customMap', mapData);

        if (x.hasOwnProperty('options')) {
          options = x.options;
        }
        if (x.hasOwnProperty('data')) {
          data = x.data;
        }
        if (x.hasOwnProperty('theme')) {
          tuiChart.registerTheme(x.theme.name, x.theme.values);
        }

        if (options.hasOwnProperty('chart') === false) {
          options.chart = {};
        }
        options.chart.width = width;
        options.chart.height = height;

        // console.log(options);
        el.innerHTML = null;
        widget = tuiChart.mapChart(el, data, options);

      },

      getWidget: function(){
        return widget;
      },

      resize: function(width, height) {

        widget.resize({width: width, height: height});

      }

    };
  }
});
