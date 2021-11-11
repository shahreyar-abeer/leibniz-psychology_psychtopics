
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
  
  // hide menu on click outside
  
  $('html').click(function(e) {
  //if clicked element is not your element and parents aren't your div
  if (e.target.id != '.sidenav' && e.target.id != '.menu' && $(e.target).parents('.sidenav').length == 0 && $(e.target).parents('.menu').length == 0 && $(window).width() < 768 && $('.sidenav').hasClass("sidenav-opened")) {
    $(".sidenav").removeClass("sidenav-opened");
    $(".main").removeClass("main-opened");
    $(".title2").removeClass("title2-opened");
  }
});
  

});
