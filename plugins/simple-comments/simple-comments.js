// add_comment button
// show comments on page load

const COMMENT_API_ROOT = "/simple-comments/";
const COMMENT_CHAR_LIMIT = 500;

function ensure_simple_comments_div(){
    if (document.getElementById("simple-comments") == null){
        const node = document.createElement("div");
        node.id = "simple-comments";
        document.getElementsByTagName("body")[0].appendChild(node);
    }
}

function load_or_refresh_comments(){
    ensure_simple_comments_div();
    let div = document.getElementById("simple-comments");
    div.innerText = "Loading comments...";
    let xhr = new XMLHttpRequest();
    let url = COMMENT_API_ROOT + `view-comment?page=${location.pathname}&parent=-1`;
    xhr.open("GET", url);
    xhr.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
    xhr.onload = (event) => {
        div.innerHTML = xhr.responseText;
        const datetimes = document.getElementsByClassName("comment-datetime");
        const date_options = {
            weekday: "short",
            year: "numeric",
            month: "short",
            day: "numeric",
            hour: "2-digit",
            minute: "2-digit"
        };
        for(let i=0; i<datetimes.length; i++){
            const date = new Date(Number(datetimes[i].innerText)*1000);
            datetimes[i].innerText = date.toLocaleDateString("en-US", date_options);
        }
    };
    xhr.send();
};

function create_reply_box_for_parent(parent=null){
    let pid = (parent == null ? "-1" : parent.id.slice(parent.id.indexOf("-")+1));
    let node = document.createElement("form");
    node.classList.add("comment-reply-form");
    node.action = "";
    node.addEventListener("submit", post_comment_and_show);
    node.innerHTML = `
<div class="comment-reply-field">
<label for="display-name">Display Name:</label>
<br/>
<input type="text" id="display-name" name="display-name" required>
</div>

  <input type="hidden" id="parent" name="parent" value=${pid}>


<div class="comment-reply-field">
<label for="reply">Comment (max. ${COMMENT_CHAR_LIMIT} char):</label>
<br/>
<textarea maxlength="${COMMENT_CHAR_LIMIT}" id="reply" name="comment" rows="4" required>
</textarea>
</div>

<div class="comment-reply-field">
<input type="submit" id="submit" name="submit" value="Post Reply">
</div>`;
    return node;
}

function enable_toplevel_reply_box(event){
    let comment = event.target.parentElement;
    let reply_box = create_reply_box_for_parent();
    event.target.remove();
    comment.classList.remove("comment-add-comment");
    comment.classList.add("comment-wrapper");
    comment.insertBefore(
        reply_box, comment.firstChild
    );
};

function enable_reply_box(event){
    let comment = event.target.parentElement.parentElement;
    let reply_box = create_reply_box_for_parent(comment);
    event.target.remove();
    comment.appendChild(reply_box);
};

function post_comment_and_show(event){
    event.preventDefault();
    let form = event.target;
    function get_comment_data(){
        return form.getElementsByTagName("textarea")[0].value;
    };
    function get_commenter_name(){
        let inputs = form.getElementsByTagName("input");
        for(let i=0; i<inputs.length; i++){
            if (inputs[i].name=="display-name")
                return inputs[i].value;
        }
    };
    function get_commenter_parent(){
        let inputs = form.getElementsByTagName("input");
        for(let i=0; i<inputs.length; i++){
            if (inputs[i].name=="parent")
                return inputs[i].value;
        }
    };

    let comment = get_comment_data();
    let name = get_commenter_name();
    let parent = get_commenter_parent();
    let xhr = new XMLHttpRequest();
    let url = COMMENT_API_ROOT + "add-comment";
    xhr.open("POST", url);
    xhr.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
    let data = {
        page: location.pathname,
        comment_data: comment,
        parent: parent,
        name: name
    };
    xhr.onload = load_or_refresh_comments;
    xhr.send(JSON.stringify(data));
};

document.addEventListener("DOMContentLoaded", load_or_refresh_comments);
