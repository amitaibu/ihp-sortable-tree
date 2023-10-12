$(document).on('ready turbolinks:load', function () {
    const container = document.getElementById('treeContainer');
    if (!container) {
        return;
    }
    const nodes = JSON.parse(container.getAttribute('data-tree'));
    initSortableTree(nodes);
});

const initSortableTree = function(nodes) {
    // Remove the old tree, by removing elements under the #treeContainer
    $('#treeContainer').empty();

    new SortableTree({
        nodes: nodes,
        initCollapseLevel: 10,
        element: document.querySelector('#treeContainer'),
        renderLabel: (data) => {
            return `<span>${data.body}</span>`;
        },

        onChange: async ({ nodes, movedNode, srcParentNode, targetParentNode }) => {
            const filterNodes = function(nodes) {
                return nodes.map(node => {
                    let newNode = {
                        element: {
                            _data: {
                                uuid: node.element._data.uuid
                            }
                        }
                    };

                    if (node.subnodes && node.subnodes.length > 0) {
                        newNode.subnodes = filterNodes(node.subnodes);
                    }
                    else  {
                        newNode.subnodes = [];
                    }

                    return newNode;
                });
            }
            const filteredNodes = filterNodes(nodes);

            try {
                const response = await fetch('/UpdateSortTasks', {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/json'
                    },
                    body: JSON.stringify({ tree: filteredNodes })
                });

                if (!response.ok) {
                    throw new Error(`HTTP error! Status: ${response.status}`);
                }

                const nodes = await response.json();
                initSortableTree(nodes);
            } catch (error) {
                console.error("There was a problem with the fetch operation:", error.message);
            }
            },
        });

    // Call IHP's delete binding, as we render the "delete" links after they have been executed.
    initDelete();
}