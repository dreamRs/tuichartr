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
        if (x.hasOwnProperty('theme')) {
          tuiChart.registerTheme(x.theme.name, x.theme.values);
        }

        if (options.hasOwnProperty('chart') === false) {
          options.chart = {};
        }
        options.chart.width = width;
        options.chart.height = height;

        el.innerHTML = null;
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


if (HTMLWidgets.shinyMode) {
  Shiny.addCustomMessageHandler('proxy-tui-chart-addData',
    function(obj) {
      var chart = get_widget(obj.id);
      if (typeof chart != 'undefined') {
        console.log(obj.data.values);
        chart.addData(obj.data.categories, obj.data.values);
      }
  });
  Shiny.addCustomMessageHandler('proxy-tui-chart-setData',
    function(obj) {
      var chart = get_widget(obj.id);
      if (typeof chart != 'undefined') {
        console.log(obj.data.values);
        chart.setData({categories: obj.data.categories, series: obj.data.values});
      }
  });
}


