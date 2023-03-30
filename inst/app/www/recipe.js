$(document).ready(function(){
  
/* Function to create list of row blocks. If we need a block with a text String
or a dropdown, make a new function here */
function createRecipeBlock(newid, df, selection, options) {
  let values = ''
  if (Array.isArray(options)) {
    values = options.map(createOption).join("")
  } else if (typeof options === "object") {
    for (i in options) {
      values = ['']
      values.push("<optgroup label='" + i + "'>");
      values.push($.map(options[i], createOption).join(""));
      values.push("</optgroup>");
    }
  } else {
    values = ''
  }
  return `<div class="form-group drop_area">` +
  `<label class="control-label ${df}" for="${newid}">${newid}</label>` +
  (selection === undefined ? `` : `<select id="${newid}" class="dropdown">` +
        `<option value="${selection}">${selection}</option>` +
          `${values}</select>`) +
        `<button class="delete">X</button>` +
            `</div>`
}

/**
  * Create dropdown menu from the array of AVISIT values
* @param {avisit} the text and value of the option
*/
function createOption(opt) {
  return `<option value="${opt}">${opt}</option>`
}

Shiny.addCustomMessageHandler('submit_recipe', function(recipe) {
  document.getElementById("droppable_blocks").innerHTML = "";
  document.getElementById("droppable_agg").innerHTML = "";
  
  if (Object.keys(recipe).includes("blocks")) {
    for(block of recipe.blocks){
      $("#droppable_blocks").append($(createRecipeBlock(block.variable, block.data, block.var_arg, block.var_options)));
      $("#droppable_agg").append($(createRecipeBlock(block.statistic, block.data, block.stat_arg, block.stat_options)));
    }
  }
  
  Shiny.setInputValue('tableGen_ui_1-recipe', recipe.title)
})
}); // $document.ready()
