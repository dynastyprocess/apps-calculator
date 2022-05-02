$(document).on('shiny:connected', (e)=> {
  $('#sever_screen').remove();

  var dpcookies = Cookies.get()
  if(dpcookies.dp_qb_type) {Shiny.setInputValue('qb_type',dpcookies.dp_qb_type);}
  if(dpcookies.dp_teams) {Shiny.setInputValue('teams',dpcookies.dp_teams);}
  if(dpcookies.dp_draft_type) {Shiny.setInputValue('draft_type', dpcookies.dp_draft_type);}
  if(dpcookies.dp_value_factor) {Shiny.setInputValue('value_factor',Number(dpcookies.dp_value_factor));}
  if(dpcookies.dp_rookie_optimism) {Shiny.setInputValue('rookie_optimism', Number(dpcookies.dp_rookie_optimism));}
  if(dpcookies.dp_future_factor) {Shiny.setInputValue('future_factor',Number(dpcookies.dp_future_factor));}

});
