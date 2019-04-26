HTMLWidgets.widget({

  name: 'tuichart',

  type: 'output',

  factory: function(el, width, height) {

    var tuiChart = tui.chart;
    var widget, options = {}, data = {};

    return {

      renderValue: function(x) {

        var chartType = tuiChart[x.type];
        if (x.hasOwnProperty('options')) {
          options = x.options;
        }
        if (x.hasOwnProperty('data')) {
          data = x.data;
        }

        if (options.hasOwnProperty('chart') === false) {
          options.chart = {};
        }
        options.chart.width = width;
        options.chart.height = height;

        widget = chartType(el, data, options);

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


// From Friss tuto (https://github.com/FrissAnalytics/shinyJsTutorials/blob/master/tutorials/tutorial_03.Rmd)
function get_widget(id){

  // Get the HTMLWidgets object
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  // Use the getWidget method we created to get the underlying widget
  var widgetObj ;

  if (typeof htmlWidgetsObj != 'undefined') {
    widgetObj = htmlWidgetsObj.getWidget();
  }

  return(widgetObj);
}


