Shiny.addCustomMessageHandler("initModal", function(msg) {
  console.log("[modal-init] received:", msg);

  var existing = document.getElementById(msg.id);
  if (existing) {
    console.log("[modal-init] reopening existing:", msg.id);
    $('#' + msg.id).modal('show');
    return;
  }

  // Build & inject the modal HTML
  var html = `
    <div class="modal fade" id="${msg.id}" tabindex="-1" role="dialog">
      <div class="modal-dialog modal-lg" style="max-width:90%; width:90%;" role="document">
        <div class="modal-content">
          <div class="modal-header">
            <h5 class="modal-title">${msg.title}</h5>
            <button type="button" class="close" data-dismiss="modal">&times;</button>
          </div>
          <div class="modal-body" style="max-height:70vh; overflow-y:auto; padding:1rem;">
            ${msg.body_html}
          </div>
          <div class="modal-footer">
            <button type="button" class="btn btn-secondary" data-dismiss="modal">Close</button>
          </div>
        </div>
      </div>
    </div>`;

  document.body.insertAdjacentHTML("beforeend", html);
  console.log("[modal-init] injected:", msg.id);

  // Bind the new inputs so Shiny picks up their values
  var modalEl = document.getElementById(msg.id);
  Shiny.unbindAll(modalEl);
  Shiny.bindAll(modalEl);

  // Show it
  $('#' + msg.id).modal('show');
  console.log("[modal-init] shown:", msg.id);
});

