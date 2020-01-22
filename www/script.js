$(function() {
  $("#sortable_agg").sortable();
  $("#sortable_agg").disableSelection();
  $(".all_blocks").sortable();
  $(".all_blocks").disableSelection();
});


/* Functions needed to render ShinyInput from the drop zones */

/**
* Rerender the contents of the shinyInput
* @param {id} the id of the dropped zone containing dropped blocks
* @param {outputID} the name of the input we'll use in Shiny as in input$outputID
*/
function setUpShiny(id, outputID) {
  var str = "";
  $('#' + id).each(function() {
    const txt = $(this).text()
    const val = $(this).parent().find("select").children("option:selected").val()
    str += "<tr><td>" + txt + (val ? ": " + val : "") + "</td></tr>";
  })
  let str_to_table = '<table>' + str + '</table>'
  Shiny.setInputValue(outputID, str_to_table)
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

domChange('droppable_agg', 'droppable_agg label', 'agg_drop_zone')
deleteBlock("droppable_agg", 'droppable_agg label', 'agg_drop_zone')
selectChange("droppable_agg", 'droppable_agg label', 'agg_drop_zone')

domChange('droppable_blocks', 'droppable_blocks label', 'block_drop_zone')
deleteBlock("droppable_blocks", 'droppable_blocks label', 'block_drop_zone')
selectChange("droppable_blocks", 'droppable_blocks label', 'block_drop_zone')


/* Functions needed to render blocks in drop zone */


/**
* Create a simple block with a name and delete button
* @param {newid} the new, unique id of the dropped block
*/
function simpleBlock(newid) {
  return `
  <div><div><div class="form-group drop_area">
  <label class="control-label" for="${newid}">
  ${newid.replace(/[0-9]/g, '').toUpperCase()}
  </label>
  <button class="delete">Delete</button>
  </div></div></div>`
}


/**
* Create a block with a dropdown menu of weeks
* @param {newid} the new, unique id of the dropped block
* @param {label} the name of the new block
*/
function selectWeekBlock(newid, label) { 
  return `<div class="form-group drop_area">
                        <label class="control-label" for="${newid}">${label}</label>
                        <select id="${newid}">
                        <option value="week1" selected>Week 1</option>
                        <option value="week2">Week 2</option>
                        <option value="week3">Week 3</option>
                        <option value="week4">Week 4</option></select>
                        <button class="delete">Delete</button>
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
      var newid = getNewId(draggableId);
      $(this).append(simpleBlock(newid));
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
          $(this).append(selectWeekBlock(newid, "Mean"));
        } else {
          $(this).append(simpleBlock(newid));
        }
    }
  }).sortable({
    revert: false
  })
});