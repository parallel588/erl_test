$(document).ready(function () {
    var crudServiceBaseUrl = '/users',
    dataSource = new kendo.data.DataSource({
        transport: {
            read:  {
                url: crudServiceBaseUrl,
                dataType: 'json'
            },
            update: {
                url: crudServiceBaseUrl,
                dataType: 'json',
                type: "PUT",
                contentType: 'application/json'
            },
            destroy: {
                url: crudServiceBaseUrl,
                dataType: 'json',
                type: "DELETE",
            },
            create: {
                url: crudServiceBaseUrl,
                dataType: 'json',
                type: 'POST'
            },
            parameterMap: function(options, operation) {
                if (operation !== 'read' && options.models) {
                    return {models: kendo.stringify(options.models)};
                }
            }
        },
        batch: true,
        pageSize: 20,
        schema: {
            model: {
                id: 'id',
                fields: {
                    id: { editable: false, nullable: true },
                    name: { validation: { required: true } },
                    email: { validation: { required: true } }
                }
            }
        }
    });

    $('#grid').kendoGrid({
        dataSource: dataSource,
        pageable: true,
        height: 430,
        toolbar: ['create'],
        columns: [
            { field:'id', title: 'id', width: '50px' },
            { field:'name', title: 'Name' },
            { field: 'email', title:'Email' },
            { command: ['edit', 'destroy'], title: '&nbsp;', width: '260px' }],
        editable: 'popup'
    });
});
