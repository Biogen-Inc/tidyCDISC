$(document).ready(function(){
  
/* Function to create list of row blocks. If we need a block with a text String
or a dropdown, make a new function here */
function simpleRecipeRowBlock(newid, df) {
  return `
  <div><div><div class="form-group drop_area">
  <label class="control-label ${df}" for="${newid}">${newid}</label>
  <button class="delete">X</button>
  </div></div></div>`
}

function selectRecipeBlock(newid, df, selection, values = '') { 
  return `<div class="form-group drop_area">
    <label class="control-label ${df}" for="${newid}">${newid}</label>
      <select id="${newid}" class="dropdown">
        <option value="${selection}">${selection}</option>
          ${values}
        </select>
          <button class="delete">X</button>
            </div>`
}

// this is used for combining block rows (on either the Var or STAT agg 
// side) scanning through the array, and appends to the html above 
// and join() concatenates it all together, joining on nothing ("")
// the df (df name as a string) remains constant
function combineRows(block_array, df) {
  let t = []
  block_array.forEach(function (blk) {
    t.push(simpleRecipeRowBlock(blk, df))
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
    t.push(selectRecipeBlock(agg_stat, df, "ALL", select_options))
  };
  t= t.join("")
  return(t)
}

// These are called arrays
const demography_rows = ["AGEGR1", "AGE",  "SEX",  "ETHNIC", "RACE", "HEIGHTBL", "WEIGHTBL"]
const demography_agg =  ["FREQ",   "MEAN", "FREQ", "FREQ",   "FREQ", "MEAN",     "MEAN"]


const ae18_rows = ["AOCCFL", "AESEV", "AESER","DTHDT"]
const ae18_agg =  ["Y_FREQ", "MAX_FREQ", "Y_FREQ", "NON_MISSING"]

const soc_pt_rows = ["AOCCFL", "AEBODSYS"]
const soc_pt_agg =  ["Y_FREQ", "NESTED_FREQ_DSC"]
const soc_pt_sel = ["NONE", "AEDECOD"]

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

$(document).on('click', '#RECIPE', function(){
/* Create custom block recipes to automatically populate when selected */
$("#RECIPE").bind("change", function(event, ui) {
  let publisher = $("#RECIPE").val();
  switch(publisher) {
    case "Table 3: Accounting of Subjects":
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("RANDFL", "ADSL")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("SAFFL", "ADSL")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("EOTSTT", "ADSL")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("EOSSTT", "ADSL")));
      
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("Y_FREQ", "ADAE")));
      $("#droppable_agg").append($(simpleRecipeRowBlock("FREQ", "ADSL")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_ABC", "ADSL", "DCTREAS")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_ABC", "ADSL", "DCSREAS")));
      break;
    case "Table 5: Demography":
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(combineRows(demography_agg, "ADSL")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(demography_rows, "ADSL")));
      break;
    case "Table 18: Overall summary of adverse events":
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(combineRows(ae18_agg, "ADAE")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(ae18_rows, "ADAE")));
      break;
    case "Table 20: Adverse events by system organ class and preferred term sorted by alphabetical order":
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_ABC", "ADAE", "AEDECOD")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEBODSYS", "ADAE")));
      break;
    case "Table 19: Adverse events by system organ class and preferred term sorted by decreasing frequency":
    case "Table 25: Severe adverse events by system organ class and preferred term":
    case "Table 29: Related adverse events by system organ class and preferred term":
    case "Table 30: Serious adverse events by system organ class and preferred term":
    case "Table 33: Related serious adverse events by system organ class and preferred term":
    case "Table 34: Adverse events that led to discontinuation of study treatment by system organ class and preferred term":
    case "Table 36: Adverse events that led to withdrawal from study by system organ class and preferred term":
    case "Table 38: Adverse events that led to drug interrupted, dose reduced, or dose increased by system organ class and preferred term":
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_DSC", "ADAE", "AEDECOD")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEBODSYS", "ADAE")));
      break;
    case "Table 21: Adverse events by system organ class":
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_DSC", "ADAE", "NONE")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEBODSYS", "ADAE")));
      break;
    case "Table 23: Adverse events by preferred term":
    case "Table 26: Severe adverse events by preferred term":
    case "Table 31: Serious adverse events by preferred term":
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_DSC", "ADAE", "NONE")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEDECOD", "ADAE")));
      break;
    case "Table 41: Blood Chemistry actual values by visit":
      var select_opts = `${bc_obj.weeks.map(createOption).join("")}`
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(oneAgg_combineSelects(bc_obj.params, "MEAN", "ADLB", select_opts)));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(bc_obj.params, "ADLB")));
      break;
    case "Table 41: Hematology actual values by visit":
      var select_opts = `${he_obj.weeks.map(createOption).join("")}`
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(oneAgg_combineSelects(he_obj.params,"MEAN","ADLB", select_opts)));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(he_obj.params, "ADLB")));
      break;
    case "Table 41: Urinalysis actual values by visit":
      var select_opts = `${ur_obj.weeks.map(createOption).join("")}`
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(oneAgg_combineSelects(ur_obj.params,"MEAN","ADLB", select_opts)));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(ur_obj.params, "ADLB")));
      break;
    default:
      document.getElementById("droppable_agg").innerHTML = "";
      document.getElementById("droppable_blocks").innerHTML = "";
  }
});

$('select#RECIPE').change(function() {
  var selectedDropdown = $(this).children('option:selected').val()
  Shiny.setInputValue('tableGen_ui_1-recipe', selectedDropdown)
})

}); // $(document).on('click', '#RECIPE'
}); // $document.ready()
