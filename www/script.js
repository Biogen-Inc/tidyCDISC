$(function() {
  $("#sortable_agg").sortable();
  $("#sortable_agg").disableSelection();
  $(".all_blocks").sortable();
  $(".all_blocks").disableSelection();
  $('#all_rows').on('shiny:value', function() {
	    setTimeout(() => $('#all_rows .all_blocks').sortable().disableSelection(), 1)
	  })
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

domChange('droppable_agg', 'droppable_agg label', 'table_generator-agg_drop_zone')
deleteBlock("droppable_agg", 'droppable_agg label', 'table_generator-agg_drop_zone')
selectChange("droppable_agg", 'droppable_agg label', 'table_generator-agg_drop_zone')

domChange('droppable_blocks', 'droppable_blocks label', 'table_generator-block_drop_zone')
deleteBlock("droppable_blocks", 'droppable_blocks label', 'table_generator-block_drop_zone')
selectChange("droppable_blocks", 'droppable_blocks label', 'table_generator-block_drop_zone')


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
                    <option value="Baseline">Baseline</option>
                    <option value="Day 1 : post-dose">Day 1 PD</option>
                    <option value="Week 2">Week 2</option>
                    <option value="Week 2 : post-dose">Week 2 PD</option>
                    <option value="Week 4">Week 4</option>
                    <option value="Week 4 : post-dose">Week 4 PD</option>
                    <option value="Week 6">Week 6</option>
                    <option value="Week 6 : post-dose">Week 6 PD</option>
                    <option value="Week 12">Week 12</option>
                    <option value="Week 24">Week 24</option>
                    <option value="Week 36">Week 36</option>
                    <option value="Week 48">Week 48</option>
                    <option value="Week 48 : post-dose">Week 48 PD</option>
                    <option value="Week 50">Week 50</option>
                    <option value="Week 50 : post-dose">Week 50 PD</option>
                    <option value="Week 52">Week 52</option>
                    <option value="Week 52 : post-dose">Week 52 PD</option>
                    <option value="Week 54">Week 54</option>
                    <option value="Week 54 : post-dose">Week 54 PD</option>
                    <option value="Week 60">Week 60</option>
                    <option value="Week 60 : post-dose">Week 60 PD</option>
                    <option value="Week 72">Week 72</option>
                    <option value="Week 84">week 84</option>
                    <option value="Week 96">Week 96</option>
                    <option value="Week 100">Week 100</option>
                    <option value="Year 1">Year 1</option>
                    <option value="Year 2">Year 2</option>
                    </select>
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
          $(this).append(selectWeekBlock(newid, "MEAN"));
        } else {
          $(this).append(simpleBlock(newid));
        }
    }
  }).sortable({
    revert: false
  })
});