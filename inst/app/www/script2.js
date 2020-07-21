$( document ).ready(function() {
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
  val = $(this).parent().find("select").children("option:selected").val()
  str += `${df}*${txt.replace(" ", "")}*${val} + `.replace(/\r?\n|\r/g, "")
  obj.numbers.push({txt,df,val})
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
  * Create a simple block with a name and delete button
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

Shiny.addCustomMessageHandler('my_data', function(df) {
  // the dataframe column is imported as an array
  my_array = Object.values(df)
  select = `${my_array.map(createOption).join("")}`
  
  $(function() {
    $(".draggable_agg").draggable();
    $("#droppable_agg").droppable({
      accept: ".agg",
      drop: function(event, ui) {
        var draggableId = ui.draggable.attr("id");
        var newid = getNewId(draggableId);
        if (draggableId.includes("anova")) {
          $(this).append(selectWeekBlock(newid, "ANOVA", select));
        } else if (draggableId.includes("chg")) {
          $(this).append(selectWeekBlock(newid, "CHG", select));
        } else if (draggableId.includes("mean")) {
          $(this).append(selectWeekBlock(newid, "MEAN", select));
        } else {
          $(this).append(simpleBlock(newid, "df"));
        }
      }
    }).sortable({
      revert: false
    })
  });
});

/**
  * Create a block with a dropdown menu of weeks
* @param {newid} the new, unique id of the dropped block
* @param {label} the name of the new block
*/
  function selectWeekBlock(newid, label, values) { 
    return `<div class="form-group drop_area">
      <label class="control-label" for="${newid}">${label}</label>
        <select id="${newid}" class="dropdown">
          <option value="NONE">NONE</option>
            ${values}
          </select>
            <button class="delete">X</button>
              </div>`
  }


/**
  * Create newIDs for all blocks dragged into drop zone containing a select box
* @param {type} the draggable element to get a new ID for
*/
  function getNewId(type) {
    var newId;
    newId = $('#droppable').find('select').length
    return type + (newId + 1);
  }

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
      $(this).append(simpleBlock(newid, df));
    }
  }).sortable({
    revert: false
  })
})


// for agg blocks, 
// create dropdowns specific to each block
$(function() {
  $(".draggable_agg").draggable();
  $("#droppable_agg").droppable({
    accept: ".agg",
    drop: function(event, ui) {
      var draggableId = ui.draggable.attr("id");
      var newid = getNewId(draggableId);
      if (draggableId.includes("ttest")) {
        $(this).append(selectWeekBlock(newid, "T-TEST"));
      } else if (draggableId.includes("chg")) {
        $(this).append(selectWeekBlock(newid, "CHG"));
      } else if (draggableId.includes("mean")) {
        $(this).append(selectWeekBlock(newid, "MEAN"));
      } else {
        $(this).append(simpleBlock(newid));
      }
    }
  }).sortable({
    revert: false
  })
});

$("#popExp_ui_1-adv_filtering").parent().parent().addClass('custom_checkbox');
$("#popExp_ui_1-adv_filtering").parent().parent().parent().addClass('custom_shiny_width');

/*Change file input css */
  $('#dataUpload_ui_1-pilot').on('click',  function () {
    $(".btn-file").addClass('disable_button');
  });
});