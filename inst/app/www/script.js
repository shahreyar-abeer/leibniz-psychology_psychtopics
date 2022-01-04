
$(document).ready(function() {
  
  // activate the slider on start
  Shiny.addCustomMessageHandler('setSlider', function(arg) {
    Shiny.setInputValue(arg.id, arg.vals);
  })
  
  // click go once a year range has been selected with the slider
  Shiny.addCustomMessageHandler('clickGo', function(arg) {
    $("#" + arg.button).click();
    console.log("#" + arg.button);
  })
  
  // pick a topic from the people picker on start
  Shiny.addCustomMessageHandler('pickOne', function(arg) {
    $(".ms-BasePicker-input").click();
    $("#sug-0").click();
    console.log("clicked")
  })
  
  // handler for tag picker
  Shiny.addCustomMessageHandler(
    type = 'updateTopicIds', function(arg) {
      window.topicIds = arg.values.map((topic, index) => ({ key: (index + 1), name: topic }) );
      Shiny.setInputValue('topic_evol-search', topicIds.slice(0, 1));
      console.log("updated")
  });
  






  
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


// pure js

// functions for tag picker
function listContainsTagList(tag, tagList) {
  if (!tagList || !tagList.length || tagList.length === 0) {
    return false;
  }
  return tagList.some(compareTag => compareTag.key === tag.key);
};

function filterSuggestedTags(filterText, tagList) {
  return filterText
    ? topicIds.filter(
        tag => tag.name.toLowerCase().includes(filterText.toLowerCase()) && !listContainsTagList(tag, tagList),
      )
    : [];
};