$(document).on('ready turbolinks:load', function () {
    const container = document.getElementById('treeContainer');
    if (!!container) {
        const nodes = JSON.parse(container.getAttribute('data-tree'));

        const tree = new SortableTree({
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

                    // Remove the Delete from the parent, but not from the child.
                    $(targetParentNode).find('.js-delete:first-child').remove();

                } catch (error) {
                    console.error("There was a problem with the fetch operation:", error.message);
                }
                },
                onClick: (event, node) => {
                    // console.log(node.data);
                },
            });

            // Call IHP's delete binding, as we render the "delete" links after they have been executed.
        initDelete();

    }
});