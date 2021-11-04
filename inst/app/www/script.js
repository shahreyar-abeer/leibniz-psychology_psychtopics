
$(document).ready(function() {
  
  
  //$("#start-dropdown_most_popular2").hide();
  
  jsmodule['@fluentui/react'].registerIcons({
    icons: {
      //'Cap': <Icon icon={'link'} />,
      Cap: React.createElement('Icon', {icon: "link"}),
      'Ups': '\uE417'
      //Filters: <FontAwesomeIcon icon={faFilter} />
      //'HomeSolid': <Icon icon={['fas', 'home']} />,
    }
  });
  
  //console.log(React.createElement('Icon', {icon: 'fa-link'}));
  
  // menu for small screens
  $('#menu').click(function(e) {
    //console.log("clicked");
    
    $(".sidenav").toggleClass("sidenav-opened");
    $(".main").toggleClass("main-opened");
    $(".title2").toggleClass("title2-opened");
    //$('.grid-container').css({ "grid-template-columns" : "50% 50%", "transition": "all 1s" });
    //$('.sidenav').css({ "width": "100%", "transition": "all 1s" })
  });
  
  //var chart = $('#htmlwidget_container').highcharts();
  //chart.series[0].data[1].select(true, true);

});
