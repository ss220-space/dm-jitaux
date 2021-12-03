opcodes = new Map();
function updateOpcodes(element) {
    diff = $(element).hasClass("opcode") ? 1 : -1;
    for (const [opcode, occurrences] of Object.entries(data.proc_data[$(element).data("proc")].opcodes)) {
        opcode_data = opcodes.get(opcode);
        if (opcode_data == null) {
            opcode_data = { procs: 0, count: 0 }
            opcodes.set(opcode, opcode_data);
        }
        opcode_data.procs = opcode_data.procs + diff;
        opcode_data.count += occurrences * diff;
    }
}
function buildEntry(id) {
    entry = data.proc_data[id];

    perf_string = ""
    if (entry.performance != null) {
        perf_string = `<span>${entry.performance.total_cpu}</span><span>${entry.performance.self_cpu}</span><span>${entry.performance.calls}</span>`
    }
    return `<li class="call" data-proc='${id}'>${entry.name}<span class='perf'>${perf_string}</div></li>`;
}
function rebuildOpcodeDisplay() {
    $("#opcodes tbody").empty();
    opcode_array = Array.from(opcodes.entries());
    opcode_array.sort(function (a, b) {
        return b[1].procs - a[1].procs;
    });

    for (const [key, value] of opcode_array) {
        if (value.procs > 0) {
            $("#opcodes tbody").append(`<tr><td>${key}</td><td>${value.procs}</td><td>${value.count}</td></tr>`);
        }
    }
}
function handleCallClick(element, event) {

    id = $(element).data("proc");

    if (event.ctrlKey) {
        $(element).toggleClass("opcode");
        updateOpcodes(element);
        rebuildOpcodeDisplay();
        return;
    } else {
        $(element).toggleClass("unfold");
    }

    if (!$(element).hasClass("unfold")) {
        for (const child of $(element).find("li.call.opcode")) {
            $(child).removeClass("opcode");
            updateOpcodes(child);
        }
        $(element).children("ul").remove();
        rebuildOpcodeDisplay();
        return;
    }

    $(element).append("<ul></ul>");
    nested = $(element).children("ul");
    for (const ref of data.proc_ref[id]) {
        el = `<li class='${ref.kind}'>`;
        if (ref.kind == "dynamic") {
            el = el + `dynamic: ${ref.title}:<ul>`;
            for (const dyn_ref of ref.refs) {
                el = el + buildEntry(dyn_ref);
            }
            el = el + "</ul></li>";
        } else if (ref.kind == "static") {
            el = buildEntry(ref.refs[0]);
        } else if (ref.kind == "unresolved") {
            el = el + `unresolved: ${ref.title}</li>`;
        }
        $(nested).append(el);
    }
    $($(element).find("li.dynamic")).click(function(event) {
        $(this).toggleClass("unfold");
        event.stopPropagation();
    });
    $($(element).find("li.call")).click(function(event) {
        event.stopPropagation();
        handleCallClick(this, event);
    });
}
$( document ).ready(function() {
    for (const root of data.roots) {
        $("ul#procs").append(buildEntry(root));
    };
    $("li.call").click(function(event) {
        event.stopPropagation();
        handleCallClick(this, event);
    });

});