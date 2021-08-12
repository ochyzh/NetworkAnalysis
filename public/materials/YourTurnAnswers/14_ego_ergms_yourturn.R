library(sna)
data(sampson)
network::list.vertex.attributes(samplike)

m2 <- ego_ergm(net = samplike,
               form = c("edges", "mutual",
                        'desp(.8, type="OSP")',
                        'desp(.8, type="ISP")'),
               core_size = 1,
               min_size = 5,
               roles = 3,
               forking = FALSE,
               ncpus = 1,
               directed = TRUE,
               edge_covariates = FALSE,
               seed = 12345,
               steps = 50,
               tol = 1e-06)

table(samplike %v% "group",m2$role_assignments[,2])
table(samplike %v% "cloisterville",m2$role_assignments[,2])


plot(samplike,
        displaylabels = TRUE,
        # size of nodes based on vector vertexSize
        vertex.cex = 2*m2$role_assignments[,2],
        # color of nodes based on vertex attribute: group
        vertex.col = 'group'
)

