




function getRandomColor() {
  var letters = '0123456789ABCDEF';
  var color = '#';
  for (var i = 0; i < 6; i++) {
    color += letters[Math.floor(Math.random() * 16)];
  }
  return color;
}



function setRandomColor() {
  $("#colorpad").css("background-color", getRandomColor());
}



$(document).on('shiny:recalculating', function(event){
	console.log('recalculating')
})

$(document).on('shiny:value shiny:error', function(event) {
	console.log('shiny:value: '+event.target.id);

	setTimeout(
		function() {
			$('.js-line, .js-fill').not('.js-tozero').each(function(){
				var pos = $(this).position()
				var parentpos = $('#mainPlot').offset();
				var bBox = $(this)[0].getBBox();

				console.log(parentpos.left, parentpos.top)

				var xcenter = pos.left - parentpos.left + bBox.width/2
				var ycenter = pos.top - parentpos.top - bBox.height/2
				// var xcenter = pos.left - (pos.left - parentpos.left)
				// var ycenter = pos.top - (pos.top - parentpos.top)

				$(this).css({transformOrigin: ''+xcenter+'px '+ycenter+'px 0px'})
				$(this).css({fill: getRandomColor()});
				// console.log(pos.left + ', '+ pos.top)
				// $(this).css({transform: 'scale(.5)'})
				$(this).addClass('animate-in')
			})
	}, 500);

	// $('.js-tozero').css({display: 'none'});
	setTimeout(
		function(){
				$('.js-line, .js-fill').css({opacity: '1'});
				$('.js-line, .js-fill').not('.js-tozero').css({opacity: '0'});
				$('.js-tozero').css({visibility: 'visible'});

		}, 1750);
})