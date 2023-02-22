$(document).ready(function(){
  
/* Function to create list of row blocks. If we need a block with a text String
or a dropdown, make a new function here */
function createRecipeBlock(newid, df, selection, values = '') {
  return `<div class="form-group drop_area">` +
  `<label class="control-label ${df}" for="${newid}">${newid}</label>` +
  (selection === undefined ? `` : `<select id="${newid}" class="dropdown">` +
        `<option value="${selection}">${selection}</option>` +
          `${values}</select>`) +
        `<button class="delete">X</button>` +
            `</div>`
}

// this is used for combining block rows (on either the Var or STAT agg 
// side) scanning through the array, and appends to the html above 
// and join() concatenates it all together, joining on nothing ("")
// the df (df name as a string) remains constant
function combineRows(block_array, df) {
  let t = []
  block_array.forEach(function (blk) {
    t.push(createRecipeBlock(blk, df))
  });
  t= t.join("")
  return(t)
}

/**
  * Create dropdown menu from the array of AVISIT values
* @param {avisit} the text and value of the option
*/
function createOption(opt) {
  return `<option value="${opt}">${opt}</option>`
}

// The following function creates a stat block for every var block on
// the LHS, and creates a dropdown. The var_block and select_input arrays
// must be of the same length. Used for Table 41.
function oneAgg_combineSelects(var_block, agg_stat, df, select_options) {
  let t = Array(var_block.length);
  for(var i = 0; i < var_block.length; i += 1){
    t.push(createRecipeBlock(agg_stat, df, "ALL", select_options))
  };
  t= t.join("")
  return(t)
}

let bc_obj = null;
Shiny.addCustomMessageHandler('adlbc', function(adlbc) {
  bc_obj = adlbc;
});

let he_obj = null;
Shiny.addCustomMessageHandler('adlbh', function(adlbh) {
  he_obj = adlbh;
});

let ur_obj = null;
Shiny.addCustomMessageHandler('adlbu', function(adlbu) {
  ur_obj = adlbu;
})

let recipes_obj = null;
Shiny.addCustomMessageHandler('recipes', function(recipes) {
  recipes_obj = recipes;
})

$(document).on('click', '#RECIPE', function(){
// Create custom block recipes to automatically populate when selected 
$("#RECIPE").bind("change", function(event, ui) {
  let publisher = $("#RECIPE").children(":selected").attr("id");
  document.getElementById("droppable_blocks").innerHTML = "";
  document.getElementById("droppable_agg").innerHTML = "";
  
  if (Object.keys(recipes_obj).includes(publisher)) {
  for(block of recipes_obj[publisher].blocks){
      $("#droppable_blocks").append($(createRecipeBlock(block.variable, block.data, block.var_arg)));
      $("#droppable_agg").append($(createRecipeBlock(block.statistic, block.data, block.stat_arg)));
  }
  } else {
  switch(publisher) {
    case "tbl41_b":
      let select_opts = `${bc_obj.weeks.map(createOption).join("")}`
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(oneAgg_combineSelects(bc_obj.params, "MEAN", "ADLB", select_opts)));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(bc_obj.params, "ADLB")));
      break;
    case "tbl41_h":
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(oneAgg_combineSelects(he_obj.params,"MEAN","ADLB",he_obj.weeks)));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(he_obj.params, "ADLB")));
      break;
    case "tbl41_u":
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(oneAgg_combineSelects(ur_obj.params,"MEAN","ADLB",ur_obj.weeks)));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(ur_obj.params, "ADLB")));
      break;
    default:
      document.getElementById("droppable_agg").innerHTML = "";
      document.getElementById("droppable_blocks").innerHTML = "";
  }
  }
});

$('select#RECIPE').change(function() {
  var selectedDropdown = $(this).children('option:selected').val()
  Shiny.setInputValue('tableGen_ui_1-recipe', selectedDropdown)
})

}); // $(document).on('click', '#RECIPE'
}); // $document.ready()
