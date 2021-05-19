

Shiny.addCustomMessageHandler('chem_weeks', function(bc_wks) {
Shiny.addCustomMessageHandler('hema_weeks', function(he_wks) {
Shiny.addCustomMessageHandler('urin_weeks', function(ur_wks) {
  Shiny.addCustomMessageHandler('adlbc_params', function(bc) {
  Shiny.addCustomMessageHandler('adlbh_params', function(he) {
  Shiny.addCustomMessageHandler('adlbu_params', function(ur) {
      $(document).ready(function(){
        $(document).on('click', '#RECIPE', function(){

    
/* Function to create list of row blocks. If we need a block with a text String
or a dropdown, make a new function here */
function simpleRecipeRowBlock(newid, df) {
  return `
  <div><div><div class="form-group drop_area">
  <label class="control-label ${df}" for="${newid}">${newid}</label>
  <button class="delete">X</button>
  </div></div></div>`
}
function selectRecipeBlock(newid, df, values) { 
    return `<div class="form-group drop_area">
      <label class="control-label ${df}" for="${newid}">${newid}</label>
        <select id="${newid}" class="dropdown">
          <option value="${values}">${values}</option>
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


/*
// this similar to combineRows above, but will add additional array into
// the mix and loops through that too, creating a var block for every combo
// between the two arrays. Depracated.
function multiplyCombineRows(var_block, df, select_input) {
  let t = []
  var_block.forEach(function (blk) {
    select_input.forEach(function () {
      t.push(simpleRecipeRowBlock(blk, df))
    });
  });
  t= t.join("")
  return(t)
}
*/


/*
// this is selecting one stat block (that contains a selectInput) 
// and then scans sets the selection options/choices to only have one
// value - one stat block per option choices. For example, it can Create
// a mean block for every week that exists in the study. Helpful for
// table 41 style tables of lab actuals by param and week or PK style 
// tables. Depracated.
function oneAgg_eachOption(var_block, agg_stat, df,  select_input) {
  let t = [];
  var_block.forEach(function () {
    select_input.forEach(function (sel_input) {
      t.push(selectRecipeBlock(agg_stat, df, sel_input));
    });
  });
  t= t.join("");
  return(t);
}
*/


// The following function creates a stat block for every var block on
// the LHS, and creates a dropdown. The var_block and select_input arrays
// must be of the same length. Used for Table 41.
function oneAgg_combineSelects(var_block, agg_stat, df, select_input) {
  let t = Array(var_block.length);
  for(var i = 0; i < var_block.length; i += 1){
    t.push(selectRecipeBlock(agg_stat, df, select_input[i]))
  };
  t= t.join("")
  return(t)
}



// These are called arrays
demography_rows = ["AGEGR1", "AGE",  "SEX",  "ETHNIC", "RACE", "HEIGHTBL", "WEIGHTBL"]
demography_agg =  ["FREQ",   "MEAN", "FREQ", "FREQ",   "FREQ", "MEAN",     "MEAN"]


ae18_rows = ["AOCCFL", "AESEV", "AESER","DTHDT"]
ae18_agg =  ["Y_FREQ", "MAX_FREQ", "Y_FREQ", "NON_MISSING"]

soc_pt_rows = ["AOCCFL", "AEBODSYS"]
soc_pt_agg =  ["Y_FREQ", "NESTED_FREQ_DSC"]
soc_pt_sel = ["NONE", "AEDECOD"]



/* Create custom block recipes to automatically populate when selected */
  $("#RECIPE").bind("change", function(event, ui) {
    var publisher = $("#RECIPE").val();
    if (publisher === "Table 3: Accounting of Subjects") {
    
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("RANDFL", "ADSL")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("TRTSDT", "ADSL")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("EOTSTT", "ADSL")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("EOSSTT", "ADSL")));
      
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("Y_FREQ", "ADAE")));
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADSL")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_DSC", "ADSL", "DCTREAS")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_DSC", "ADSL", "DCSREAS")));
    } else if (publisher === "Table 5: Demography") {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(combineRows(demography_agg, "ADSL")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(demography_rows, "ADSL")));
      
      
    } else if (publisher === "Table 18: Overall summary of adverse events") {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(combineRows(ae18_agg, "ADAE")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(ae18_rows, "ADAE")));
      
      
    }  else if (publisher === "Table 20: Adverse events by system organ class and preferred term sorted by alphabetical order") {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_ABC", "ADAE", "AEDECOD")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEBODSYS", "ADAE")));
      
      
    }
    else if (["Table 19: Adverse events by system organ class and preferred term sorted by decreasing frequency",
                "Table 25: Severe adverse events by system organ class and preferred term",
                "Table 29: Related adverse events by system organ class and preferred term",
                "Table 30: Serious adverse events by system organ class and preferred term",
                "Table 33: Related serious adverse events by system organ class and preferred term",
                "Table 34: Adverse events that led to discontinuation of study treatment by system organ class and preferred term",
                "Table 36: Adverse events that led to withdrawl from study by system organ class and preferred term",
                "Table 38: Adverse events that led to drug interrupted, dose reduced, or dose increased by system organ class and preferred term"].includes(publisher)) {
                  
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_DSC", "ADAE", "AEDECOD")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEBODSYS", "ADAE")));
      
      
    } else if (publisher === "Table 21: Adverse events by system organ class") {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_DSC", "ADAE", "NONE")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEBODSYS", "ADAE")));
      
      
    } else if (["Table 23: Adverse events by preferred term",
                "Table 26: Severe adverse events by preferred term",
                "Table 31: Serious adverse events by preferred term"].includes(publisher)) {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_DSC", "ADAE", "NONE")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEDECOD", "ADAE")));
      
     } else if (publisher === "Table 41: Blood Chemistry actual values by visit"){
       
       c_params = Object.values(bc);
       c_weeks = Object.values(bc_wks);
       document.getElementById("droppable_agg").innerHTML = "";
       //$("#droppable_agg").append($(oneAgg_eachOption(c_params,"MEAN","ADLB",weeks)));
       $("#droppable_agg").append($(oneAgg_combineSelects(c_params,"MEAN","ADLB",c_weeks)));
       document.getElementById("droppable_blocks").innerHTML = "";
       //$("#droppable_blocks").append($(multiplyCombineRows(c_params, "ADLB", weeks)));
       $("#droppable_blocks").append($(combineRows(c_params, "ADLB")));

     } else if (publisher === "Table 41: Hematology actual values by visit"){
       
       h_params = Object.values(he);
       h_weeks = Object.values(he_wks);
       document.getElementById("droppable_agg").innerHTML = "";
       //$("#droppable_agg").append($(oneAgg_eachOption(h_params,"MEAN","ADLB",weeks)));
       $("#droppable_agg").append($(oneAgg_combineSelects(h_params,"MEAN","ADLB",h_weeks)));
       document.getElementById("droppable_blocks").innerHTML = "";
       //$("#droppable_blocks").append($(multiplyCombineRows(h_params, "ADLB", weeks)));
       $("#droppable_blocks").append($(combineRows(h_params, "ADLB")));
       
     } else if (publisher === "Table 41: Urinalysis actual values by visit"){

        //document.getElementById("droppable_agg").innerHTML = "";
        //$("#droppable_agg").append($(combineRows(demography_agg, "ADSL")));
        //document.getElementById("droppable_blocks").innerHTML = "";
        //$("#droppable_blocks").append($(combineRows(demography_rows, "ADSL")));
       u_params = Object.values(ur);
       u_weeks = Object.values(ur_wks);
       document.getElementById("droppable_agg").innerHTML = "";
       //$("#droppable_agg").append($(oneAgg_eachOption(u_params,"MEAN","ADLB",weeks)));
       $("#droppable_agg").append($(oneAgg_combineSelects(u_params,"MEAN","ADLB",u_weeks)));
       document.getElementById("droppable_blocks").innerHTML = "";
       //$("#droppable_blocks").append($(multiplyCombineRows(u_params, "ADLB", weeks)));
       $("#droppable_blocks").append($(combineRows(u_params, "ADLB")));
      
    } else {
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

}); // end of adlbu_params
}); // end of adlbh_params
}); // end of adlbc_params
}); // end of urin weeks
}); // end of hema weeks
}); // end of chem weeks

