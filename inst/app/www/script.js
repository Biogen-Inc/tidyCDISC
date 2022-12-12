$( document ).ready(function() {
  // setup sortable and draggable functionality
  $(function() {
    $("#sortable_agg").sortable();
    $("#sortable_agg").disableSelection();
    $(".all_blocks").sortable();
    $(".all_blocks").disableSelection();
    $('#all_rows').on('shiny:value', function() {
      setTimeout(() => $('#all_rows .all_blocks').sortable().disableSelection(), 1)
    })
    $('#all_rows').on('shiny:value', function() {
      setTimeout(() => 	$(".block").draggable({
        revert: "invalid" ,
        helper: function(){
          $copy = $(this).clone();
          return $copy;},
        appendTo: 'body',
        scroll: false
      }), 1)
    })
  });
  
  
  /* Functions needed to render ShinyInput from the drop zones */
    
/**
  * Rerender the contents of the shinyInput
* @param {id} the id of the dropped zone containing dropped blocks
* @param {outputID} the name of the input we'll use in Shiny as in input$outputID
*/
  function setUpShiny(id, outputID, obj) {
    var obj = { numbers: [] }
    var str = "";
    $('#' + id).each(function() {
      txt = $(this).text()
      df = $(this).attr("class").split(" ")[1]
      grp = $(this).parent().find(":selected").parent().attr("label")
      val = $(this).parent().find(":selected").val()
      lst = [];
      if (val === "ALL") {
      for (let i = 0; i < $(this).parent().find(":selected").parent().children().length; i++) {
        if (["NONE", "ALL"].includes($(this).parent().find(":selected").parent().children()[i].text)) { continue; }
        lst.push($(this).parent().find(":selected").parent().children()[i].text);
      }
      }
      str += `${df}*${txt.replace(" ", "")}*${val} + `.replace(/\r?\n|\r/g, "")
      obj.numbers.push({txt,df,grp,val,lst})
    })
  // currently return a string seperated by +
    // and blocks must be one word - this is very fragile!
    // would be much better to return an object!
    //console.log(obj.numbers.push({df,txt,val}))
    Shiny.setInputValue(outputID, obj)
  }

/**
  * Delete block from drop zone
* @param {id} the id of the dropped zone of palette blocks
* @param {inputID} the label for the id of the dropped zone of palette blocks
* @param {outputID} the name of the input we'll use in Shiny as in input$outputID
*/
function deleteBlock(id, inputID, outputID) {
$('#' + id).on('click', '.delete', function() {
$(this).parent().remove();
setUpShiny(inputID, outputID)
})
}

/**
  * Detect changes in select drop downs
* @param {id} the id of the dropped zone of palette blocks
* @param {inputID} the label for the id of the dropped zone of palette blocks
* @param {outputID} the name of the input we'll use in Shiny as in input$outputID
*/
function selectChange(id, inputID, outputID) {
$('#' + id).change(function() {
setUpShiny(inputID, outputID)
})
}

/**
  * Detect changes in the DOM (blocks added or sorted)
* @param {id} the id of the dropped zone of palette blocks
* @param {inputID} the label for the id of the dropped zone of palette blocks
* @param {outputID} the name of the input we'll use in Shiny as in input$outputID
*/
function domChange(id, inputID, outputID) {
var target = document.querySelector('#' + id)
var observer = new MutationObserver(function(mutations) {
  setUpShiny(inputID, outputID) })
var config = {
  attributes: true,
  childList: true,
  characterData: true
};
observer.observe(target, config);
}

domChange('droppable_agg', 'droppable_agg label', 'tableGen_ui_1-agg_drop_zone')
deleteBlock("droppable_agg", 'droppable_agg label', 'tableGen_ui_1-agg_drop_zone')
selectChange("droppable_agg", 'droppable_agg label', 'tableGen_ui_1-agg_drop_zone')

domChange('droppable_blocks', 'droppable_blocks label', 'tableGen_ui_1-block_drop_zone')
deleteBlock("droppable_blocks", 'droppable_blocks label', 'tableGen_ui_1-block_drop_zone')
selectChange("droppable_blocks", 'droppable_blocks label', 'tableGen_ui_1-block_drop_zone')


/* Functions needed to render blocks in drop zone */
  
  
  /**
  * Create a simple block just contains a label and X button
* @param {newid} the new, unique id of the dropped block
*/
  function simpleBlock(newid, df) {
    return `
    <div><div><div class="form-group drop_area">
      <label class="control-label ${df}" for="${newid}">
        ${newid.slice(0, -1).toUpperCase()}
      </label>
        <button class="delete">X</button>
          </div></div></div>`
  }

/**
  * Create dropdown menu from the array of AVISIT values
* @param {avisit} the text and value of the option
*/
  function createOption(opt) {
    return `<option value="${opt}">${opt}</option>`
  }



/**
  * Function that brings in vectors from shiny and uses 
  * them to create the appropriate style block for the agg chosen
*/
// Now if there is any bds datasets also loaded, run this function
Shiny.addCustomMessageHandler('my_weeks', function(df) {
    // the dataframe column is imported as an array
    weeks_array = Object.values(df)
    week_opts = `${weeks_array.map(createOption).join("")}`
    //console.log("weeks_array[0]:", weeks_array[0])
    

    Shiny.addCustomMessageHandler('all_cols', function(cols) {
      
      // the dataframe column is imported as an array
      col_array = Object.values(cols)
      col_opts = `${col_array.map(createOption).join("")}`
  
  
/**
  * A function to run if no BDS dataframes are loaded: leave
  * week options blank since their default method is null and remove the
  * rows for the mean block since we don't need a dropdown for week
*/
    // if weeks array is undefined, then do all cols version, else all cols and weeks_array
    if (weeks_array[0] === "fake_weeky") {
      // no weeks, just col dropdowns
    $(function() {
      $(".draggable_agg").draggable();
      $("#droppable_agg").droppable({
        accept: ".agg",
        drop: function(event, ui) {
          var draggableId = ui.draggable.attr("id");
          var newid = getNewId(draggableId);
          if (draggableId.includes("anova")) {
            $(this).append(selectBlock(newid, "ANOVA"));
          } else if (draggableId.includes("chg")) {
            $(this).append(selectBlock(newid, "CHG"));
          //} else if (draggableId.includes("mean")) {
          //  $(this).append(selectBlock(newid, "MEAN"));
          } else if (draggableId.includes("nested_freq_dsc")) {
            $(this).append(selectBlock(newid, "NESTED_FREQ_DSC", col_opts));
          } else if (draggableId.includes("nested_freq_abc")) {
            $(this).append(selectBlock(newid, "NESTED_FREQ_ABC", col_opts));
          } else {
            $(this).append(simpleBlock(newid, "df"));
          }
        }
      }).sortable({
        revert: false
      })
    }); // end all_cols only function(), ie, no weeks!
    
      
    } else { // Weeks exist in the function below
    
/**
  * A function to run if BDS dataframes are loaded and a
  * week option needs to be created for some agg blocks
*/
    $(function() {
      $(".draggable_agg").draggable();
      $("#droppable_agg").droppable({
        accept: ".agg",
        drop: function(event, ui) {
          var draggableId = ui.draggable.attr("id");
          var newid = getNewId(draggableId);
          if (draggableId.includes("anova")) {
            $(this).append(selectBlock(newid, "ANOVA", week_opts));
          } else if (draggableId.includes("chg")) {
            $(this).append(selectBlock(newid, "CHG", week_opts));
          } else if (draggableId.includes("mean")) {
            $(this).append(selectBlock(newid, "MEAN", week_opts));
          } else if (draggableId.includes("nested_freq_dsc")) {
            $(this).append(selectBlock(newid, "NESTED_FREQ_DSC", col_opts));
          } else if (draggableId.includes("nested_freq_abc")) {
            $(this).append(selectBlock(newid, "NESTED_FREQ_ABC", col_opts));
          } else {
            $(this).append(simpleBlock(newid, "df"));
          }
        }
      }).sortable({
        revert: false
      })
    }); // end all_cols and weeks function()
    
    } // end of if-then-else
  }); // end "all_cols"  handler
}); // end "my_weeks" handler


/**
  * Create a block with a dropdown menu of weeks
* @param {newid} the new, unique id of the dropped block
* @param {label} the name of the new block
*/
  function selectBlock(newid, label, values, df = "") { 
    return `<div class="form-group drop_area">
      <label class="control-label ${df}" for="${newid}">${label}</label>
        <select id="${newid}" class="dropdown">
          <option value="NONE">NONE</option>
            ${values}
          </select>
            <button class="delete">X</button>
              </div>`
  }


/**
  * Create newIDs for all blocks dragged into drop zone containing
  * a select box
* @param {type} the draggable element to get a new ID for
*/
  function getNewId(type) {
    var newId;
    newId = $('#droppable').find('select').length
    return type + (newId + 1);
  }

/**
 * Function to import multiple avals per visit 
*/
let atpt_array = null;
let atpt_opts = null;
const atpt_avals = ["DIABP", "SYSBP", "PULSE"];
Shiny.addCustomMessageHandler('my_avals', function(atpt) {
  atpt_array = Object.values(atpt);
  atpt_opts = `${atpt_array.map(createOption).join("")}`;
});

// on block dropdown create simple blocks 
// with the block names from the droppable area
// and delete buttons
$(function() {
  $(".blocks").draggable();
  $('#droppable_blocks').droppable({
    accept: ".block",
    drop: function(event, ui) {
      var draggableId = ui.draggable.attr("id");
      var df = ui.draggable.closest('ul')[0].classList[1]
      var newid = getNewId(draggableId);
      if (atpt_array !== null && atpt_avals.some(el => draggableId.includes(el))) {
        $(this).append(selectBlock(newid, newid.slice(0, -1).toUpperCase(), atpt_opts, df));
      } else {
        $(this).append(simpleBlock(newid, df));
      }
    }
  }).sortable({
    revert: false
  })
})






/*
// commented out Dec 9 on commit 937a4e8
// for agg blocks, 
// create dropdowns specific to each block
$(function() {
  $(".draggable_agg").draggable();
  $("#droppable_agg").droppable({
    accept: ".agg",
    
    drop: function(event, ui) {
      var draggableId = ui.draggable.attr("id");
      var newid = getNewId(draggableId);
      if (draggableId.includes("anova")) {
        $(this).append(selectBlock(newid, "ANOVA"));
      } else if (draggableId.includes("chg")) {
        $(this).append(selectBlock(newid, "CHG"));
      } else if (draggableId.includes("mean")) {
        $(this).append(selectBlock(newid, "MEAN"));
      } else if (draggableId.includes("nested_freq_dsc")) {
        $(this).append(selectBlock(newid, "NESTED_FREQ_DSC"));
      } else if (draggableId.includes("nested_freq_abc")) {
        $(this).append(selectBlock(newid, "NESTED_FREQ_ABC"));
      } else {
        $(this).append(simpleBlock(newid, "df"));
      }
    }
    
  }).sortable({
    revert: false
  })
});
*/

$("#popExp_ui_1-adv_filtering").parent().parent().addClass('custom_checkbox');
$("#popExp_ui_1-adv_filtering").parent().parent().parent().addClass('custom_shiny_width');

/*Change file input css */
  $('#dataUpload_ui_1-pilot').on('click',  function () {
    $("#dataUpload_ui_1-file").parent(".btn-file").addClass('disable_button');
  });
});