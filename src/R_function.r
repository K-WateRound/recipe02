# ------------------------------------------------------------------------------------
# 번  호: 1
# 함수명: ts_graph_fn
# 입력값: input_df(그래프 작성을 위한 데이터), input.scales(축 범위 고정 혹은 변동)
# 설  명: ggplot을 이용한 시계열 그래프(line graph) 작성
# ------------------------------------------------------------------------------------

ts_graph_fn <- function(input_df, input.scales = c('free_x', 'free')){
    # 그래프 작성을 위한 데이터 변환
    sel_reshape_df <- reshape(data = input_df, idvar = date.var,
                              varying = names(input_df)[-1],
                              v.name = c('Value'),
                              times = names(input_df)[-1],
                              direction = 'long')
    names(sel_reshape_df) <- c('Date', 'Variable', 'Value')
    row.names(sel_reshape_df) <- NULL
    tail(sel_reshape_df)
  
    # ggplot을 이용한 시계열 그래프 작성
    var.x <- 'Value'
    group.var <- 'Variable'

    sel_reshape_df %>%
      ggplot() + 
      geom_line(aes_string(x = date.var, y = var.x)) +
      facet_wrap(facets = as.formula(paste('~', group.var)), scales = input.scales, labeller = label_both) +
      theme(strip.text.x = element_text(size = 8), axis.title = element_text(size = 8), axis.text = element_text(size = 6)) 
}


# ------------------------------------------------------------------------------------
# 번  호: 2
# 함수명: make_lag_data_fn
# 입력값: data(변수선택 할 데이터), y(종속변수), times(선행(lead) 또는 지연(lag) 할 시점)
# 설  명: 데이터 시점 지연 실시
# ------------------------------------------------------------------------------------

make_lag_data_fn <- function(data, y = column, times = 3){
    # 시점 지연
    input_df <- as.data.table(data)
    X <- data.table(input_df[, y, with = F], # 종속변수(y)의 열만 추출
                    input_df[, shift(.SD, 1:times, NA, 'lag', TRUE), .SDcols = 1:ncol(input_df)])  # 변수별 times게수만큼 lag열 생성 
                    # .SD:벡터(원본데이터)(.SD는 그룹핑 칼러을 제외한 모든 칼럼을 의미함)
                    # 1:times: 몇번째 이전(이후) 값을 가져올건지 설정, # NA: 채울값이 없을경우 default값 설정
                    # lag: 이전 값을 가져올지 이후 값을 가져올지 type설정, True: 값이름 지정유무
                    # .SDcols1: 연산대상이 되는 특정칼럼을 지정 
    X <- na.omit(X)  # 결측치 행 제거    
    
    return(X)
}


# ------------------------------------------------------------------------------------
# 번  호: 3
# 함수명: standard_fn
# 입력값: data(표준화시킬 데이터), input.mean(표준화에 사용될 평균), input.sd(표준화에 사용될 표준편차)
# 설  명: 데이터 표준화
# ------------------------------------------------------------------------------------

standard_fn <- function(data, input.mean, input.sd){
    input_df <- as.data.frame(data)
    col.names <- names(input.mean)
 
    for (i in 1:length(col.names)){
        input_df[, col.names[i]] <- (input_df[, col.names[i]] - input.mean[col.names[i]])/input.sd[col.names[i]]  # 표준화
    }
    return(input_df)
}


# ------------------------------------------------------------------------------------
# 번  호: 4
# 함수명: rearray_fn
# 입력값: data(재배열할 데이터), times(선행(lead) 또는 지연(lag)할 시점), ds(데이터 차원)
# 설  명: 입력받은 차원(dim)을 바탕으로 데이터 재배열
# ------------------------------------------------------------------------------------

rearray_fn <- function(data, times, ds){
    if (ds[2] / times > 1){  # (데이터 열의 개수/times)가 1보다 크면
        data <- array(data, c(ds[1], times, (ds[2] / times)))  # array(데이터, c(행의개수, 열의개수, 리스트개수))
    }else{  # (데이터 열의 개수/times)가 1보다 작거나 같으면
        data <- array(data, c(ds[1], times, 1))  # array(데이터, c(행의개수, 열의개수, 1)) 
    }
    return(data)
} 


# ------------------------------------------------------------------------------------
# 번  호: 5
# 함수명: remake_fn
# 입력값: input.train(훈련용 데이터), input.test(테스트용 데이터), times(선행(lead) 또는 지연(lag)할 시점)
# 설  명: Nerual Network용 데이터(machine = F)를 만들기 위한 데이터 재배열
# ------------------------------------------------------------------------------------
      
remake_fn <- function(input.train, input.test, times = 1){
    train <- input.train
    test <- input.test
    
    train.dim <- dim(train)
    test.dim  <- dim(test)
    
    vector.train <- melt.data.table(train, measure.vars = 1:ncol(train))[, value]
    vector.test  <- melt.data.table(test, measure.vars = 1:ncol(test))[, value]
    
    rearray.train <- rearray_fn(vector.train, times = times, ds = train.dim)
    rearray.test <- rearray_fn(vector.test, times = times, ds = test.dim)
    
    return(list(train = rearray.train, test = rearray.test))
}      


# ------------------------------------------------------------------------------------
# 번  호: 6
# 함수명: make_data_fn
# 입력값: data(변수선택 할 데이터), y(종속변수), times(선행(lead) 또는 지연(lag) 할 시점)
# 설  명: 변수선택법(후진선택법, Boruta 알고리즘, 모든 변수) 실시
# ------------------------------------------------------------------------------------

make_data_fn <- function(data, y = column, times = 1, machine = F){
    X <- data
    train <- as.data.table(X[['train']])
    test <- as.data.table(X[['test']])  
    
    # 변수선택
    formula <- formula(paste(names(train)[which(y == names(train))], '~', '.'))  # 형태: 종속변수 ~ .
    result_bwd  <- stepAIC(lm(formula, train), direction = c('backward'), trace = 0)  # 후진선택법(여기서 trace는 중간과정을 보여줄 것인지를 설정)
    result_boruta <- Boruta(formula, data = train, doTrace = 0, maxRuns = 500)  # Boruta 알고리즘(doTrace: 정밀도 및 알고리즘이 달라짐, 
                                                                                #                maxRuns: run횟수가 maxrun을 넘을때까지 위 알고리즘 반복)
  
    # 변수선택법에 따른 데이터셋 만들기
    var.bwd <- c(names(result_bwd$coefficient)[-1])  # 후진선택법으로 추출된 변수명
    var.boruta <- c(names(result_boruta[[1]][result_boruta[[1]] == 'Confirmed']))  # Boruta로 추출된 변수 설정
  
    x_bwd <- list(train = train[, var.bwd, with = F], test = test[, var.bwd, with = F])
    x_boruta <- list(train = train[, var.boruta, with = F], test = test[, var.boruta, with = F])
    x_full <- list(train = train[, -y, with = F], test = test[, -y, with = F])
    y <- list(train = train[, y, with = F], test = test[, y, with = F])   
  
    if(machine == T){  # 머신러닝 O
        return(list('x_bwd' = x_bwd, 'x_boruta' = x_boruta, 'x_full' = x_full, 'y' = y))
    }else{    # 머신러닝 X
        x_bwd <- remake_fn(x_bwd[['train']], x_bwd[['test']], times = 1) 
        x_boruta <- remake_fn(x_boruta[['train']], x_boruta[['test']], times = 1) 
        x_full <- remake_fn(x_full[['train']], x_full[['test']], times = 1) 
        y <- remake_fn(y[['train']], y[['test']], times = 1)  
        
        return(list('x_bwd' = x_bwd, 'x_boruta' = x_boruta, 'x_full' = x_full, 'y' = y)) 
    }
}


# ------------------------------------------------------------------------------------
# 번  호: 7
# 함수명: reshape_fn
# 입력값: data(훈련용/테스트용으로 분할하려고 하는 데이터), train.index(분할 기준), test.index(분할 기준),
#         times(선행(lead) 또는 지연(lag) 할 시점), i(이동 시점), moving(시점 이동 여부), 
#         machine(machine learning method 여부)
# 설  명: 훈련/테스트용 데이터 분할(list 형식)
# ------------------------------------------------------------------------------------

reshape_fn <- function(data, train.index, test.index, times = 1, i = i, moving = F, machine = F){
    if(moving == F){  # 시점이동 X
        train <- data[(1:train.index), ]
    }else{  # 시점이동 O
        train <- data[((1 + i):train.index), ]
    }
    
    if (machine == T){  # 머신러닝 O
        test <- data[(train.index + 1):(train.index + test.index), ]
        return(list(train = train, test = test))
    }else{  # 머신러닝 X
        test <- data[(train.index + 1):(train.index + test.index), ]
        remake_fn(train = train, test = test, time = times)  # 데이터 재배열
    }
}


# ------------------------------------------------------------------------------------
# 번  호: 8
# 함수명: machine_model_fn
# 입력값: grid(튜닝 파라미터), x(훈련용 데이터의 독립변수 값), y(훈련용 데이터의 종속변수 값), method(선택된 모델)
# 설  명: caret 패키지를 활용한 모델 학습
# ------------------------------------------------------------------------------------

machine_model_fn <- function(grid, x, y, method, ...){
    set.seed(825) # 결과 재생산을 위한 seed 고정
    
    x <- as.data.frame(x)
    y <- as.vector(unlist(y))
    
    model <- caret::train(
    x = x,
    y = y,
    tuneGrid = grid,
    method = method,
    ...
    )
    return(model)
}


# ------------------------------------------------------------------------------------
# 번  호: 9
# 함수명: result_fn
# 입력값: pred.train(훈련용 데이터의 예측값), pred.test(테스트용 데이터의 예측값), 
#         Y.train(훈련용 데이터의 종속변수 값), Y.test(테스트용 데이터의 종속변수 값), y.name(종속변수명), 
#         file.name(파일저장명), main.name(그래프 제목), sub.name(그래프 부제목), 
#         input.mean(정규화 전 훈련용 데이터의 평균), input.sd(정규화 전 훈련용 데이터의 표준편차), 
#         exp(지수변환 여부), moving(시점 이동 여부)
# 설  명: 모형 예측 결과(평가지표, 그래프) 저장 및 출력
# ------------------------------------------------------------------------------------

result_fn <- function(pred.train = NA, pred.test, Y.train = NA, Y.test, y.name, file.name, main.name, sub.name, input.mean, input.sd, exp = F, moving = T){
    # output폴더 생성
    input <- getwd()  # 현재경로
    input_folder <- paste0(input, '/output')
    dir.create(input_folder)  # 현재경로에서 ouput폴더 생성

    # 종속변수명 폴더 생성
    variable <- var.name  # 폴더명 이름 
    input_folder_v <- paste0(input_folder, '/', variable)
    
    for(i in 1:length(variable)){
        dir.create(input_folder_v[i])
    }

    # 지역명 폴더 생성
    region <- area.x[area.num]  # 지역명 이름 
    for(i in 1:length(region)){
        input_folder_r <- paste0(input_folder_v[i], '/', region)
        dir.create(input_folder_r[i])
    }
  
    R.train <- as.vector(unlist(Y.train)*input.sd[y.name] + input.mean[y.name]) # R.train : actual value(train set)
    P.train <- as.vector(pred.train*input.sd[y.name] + input.mean[y.name]) # P.train : predicted value(train set)
    R.test <- as.vector(unlist(Y.test)*input.sd[y.name] + input.mean[y.name]) # R.test : actual value(test set)
    P.test <- as.vector(pred.test*input.sd[y.name] + input.mean[y.name]) # P.test : predicted value(test set)

    if (exp == TRUE) {
        R.train = exp(R.train)
        P.train = exp(P.train)
        R.test = exp(R.test)
        P.test = exp(P.test)
    }

    R.data <- list(R.train, R.test)
    P.data <- list(P.train, P.test) 
    
    data.set <- c('train', 'test')
      
    if(is.na(R.data[[1]]) == TRUE & is.na(P.data[[1]]) == TRUE){  # pred.train, Y.train 값이 없을 경우
        n <- length(data.set)  # n은 2
    }else{  # 값이 있을 경우
        n <- 1:length(data.set)  # n의 범위는 1~2
    }
    
    for (i in n) { 
        ylim.value <- c(min(R.data[[i]], P.data[[i]]), max(R.data[[i]], P.data[[i]])) 
       
        # scatterplot
        # 파일명 지정
        if(moving == F) {
            png(paste0('output/', unlist(strsplit(y.name, '[.]'))[1], '/', unlist(strsplit(y.name, '[.]'))[2], '/', '[',
                       y.name, ']', file.name, paste0('_', data.set[i]), '_movingF', '_scatterplot', '.png'))
        }else{
            png(paste0('output/', unlist(strsplit(y.name, '[.]'))[1], '/', unlist(strsplit(y.name, '[.]'))[2], '/', '[',
                       y.name, ']', file.name, paste0('_', data.set[i]), '_movingT', '_scatterplot', '.png'))
        }     
        # 그래프 설정
        plot(P.data[[i]] ~ R.data[[i]], type = 'p', xlab = 'Predicted', ylab = 'Actual', 
             xlim = ylim.value, lwd = 2, ylim = ylim.value, cex = 1.5, pch= 16 )
        mtext(side = 3, main.name, line = 2, cex = 1.5)
        
        if(moving == F){  # 시점이동을 하지 않은 경우
            mtext(side = 3, paste0(sub.name, '(moving = F)'), line = 1, cex = 1.2)
        }else{  # 시점이동 한 경우
            mtext(side = 3, paste0(sub.name, '(moving = T)'), line = 1, cex = 1.2)
        }
        abline(lm(P.data[[i]] ~ R.data[[i]]))
        corr <- round(cor(R.data[[i]], P.data[[i]]), 3)
        legend(x = 'bottomright', title = 'Cor', legend = corr, cex = 1.3, box.lwd = 0)
        dev.off()
       
        # 엑셀파일로 예측결과값 저장
        result <- data.frame(actual_value = R.data[[i]], predicted_value = P.data[[i]], 
                             MAPE = MAPE(P.data[[i]], R.data[[i]]), RMSE = RMSE(P.data[[i]], R.data[[i]]))
        result <- round(result, 5)
        
        if(moving == F){
            write.csv(result, file = paste0('output/', unlist(strsplit(y.name, '[.]'))[1], '/', 
                                            unlist(strsplit(y.name, '[.]'))[2], '/', '[', y.name, ']', 
                                            file.name, paste0('_', data.set[i]), '_movingF', '.csv'), row.names = FALSE)
        }else{
            write.csv(result, file = paste0('output/', unlist(strsplit(y.name, '[.]'))[1], '/', 
                                            unlist(strsplit(y.name, '[.]'))[2], '/', '[', y.name, ']', 
                                            file.name, paste0('_', data.set[i]), '_movingT', '.csv'), row.names = FALSE)
        }
       
        # lineplot 
        # 파일명 지정
        if(moving == F){
            png(paste0('output/', unlist(strsplit(y.name, '[.]'))[1], '/', unlist(strsplit(y.name, '[.]'))[2], '/', '[',
                       y.name, ']', file.name, paste0('_', data.set[i]), '_movingF', '_lineplot', '.png'), width = 960)
        }else{
            png(paste0('output/', unlist(strsplit(y.name, '[.]'))[1], '/', unlist(strsplit(y.name, '[.]'))[2], '/', '[',
                y.name, ']', file.name, paste0('_', data.set[i]), '_movingT', '_lineplot', '.png'), width = 960)
        }
        
        # 그래프 설정 
        plot(R.data[[i]], type = 'o', ylab = y.name, xlab = 'Date', lwd = 2, ylim = ylim.value, cex = 1.5, pch= 16)
        mtext(side = 3, main.name, line = 2, cex = 1.5)
        if(moving == F){ # 시점이동을 하지 않은 경우
            mtext(side = 3, paste0(sub.name, '(moving = F)'), line = 1, cex = 1.2)
        }else{  # 시점이동 한 경우
            mtext(side = 3, paste0(sub.name, '(moving = T)'), line = 1, cex = 1.2)
        }
    
        # 예측값이 0미만의 값은 0으로 변경
        P.data[[i]][P.data[[i]] < 0] <- 0
        lines(P.data[[i]], col = 'red', type = 'o', lwd = 2, lty = 2, cex = 1.5, pch= 16)
        legend(x = 'topright', legend = c('Actual', 'Predicted'), col = c('black', 'red'), 
               lwd = 2, lty = c(1, 2), cex = 1.3)
        dev.off()      
        
        # 그래프 출력(lineplot)
        plot(R.data[[i]], type = 'o', ylab = y.name, xlab = 'Date', lwd = 2, ylim = ylim.value, cex = 1.5, pch= 16)
        mtext(side = 3, main.name, line = 2, cex = 1.5)
        if(moving == F){ # 시점이동을 하지 않은 경우
            mtext(side = 3, paste0(sub.name, '(moving = F)'), line = 1, cex = 1.2)
        }else{  # 시점이동 한 경우
            mtext(side = 3, paste0(sub.name, '(moving = T)'), line = 1, cex = 1.2)
        }
    
        # 예측값이 0미만의 값은 0으로 변경
        P.data[[i]][P.data[[i]] < 0] <- 0
        lines(P.data[[i]], col = 'red', type = 'o', lwd = 2, lty = 2, cex = 1.5, pch= 16)
        legend(x = 'topright', legend = c('Actual', 'Predicted'), col = c('black', 'red'), 
               lwd = 2, lty = c(1, 2), cex = 1.3)
    }
}


# ------------------------------------------------------------------------------------
# 번  호: 10
# 함수명: nn_fn
# 입력값: train.X(훈련용 데이터의 독립변수 값), train.Y(훈련용 데이터의 종속변수 값), 
#         batch.size(훈련용 데이터를 여러 작은 그룹으로 나누었을 때 하나의 소그룹에 속하는 데이터 수), 
#         units(layer 안의 node의 수), pred(출력node의 수)
# 설  명: NN모델 학습
# ------------------------------------------------------------------------------------

nn_fn <- function(train.X, train.Y, batch.size = 100, units = 30, pred = 1){
    callbacks <- list(callback_early_stopping(patience = 200, monitor = 'loss')) 
    model <- keras_model_sequential()
    batch.size <- batch.size
  
    model %>% 
      layer_dense(input_shape  = c(1, dim(train.X)[3]), units = units, activation = 'relu') %>%
      layer_dense(units = units, activation = 'relu') %>%
      layer_dense(units = 1) # %>% 
      # time_distributed(layer_dense(units = pred))  # 입력의 모든 시간 조각에 레이어를 적용
  
  
    model %>%
      compile(loss = 'mse',
              # optimizer = optimizer,
              optimizer = optimizer_rmsprop()
              # (metrics = list('mean_squared_error')
              )
  
    history <- model %>% fit(x = train.X, y = train.Y,
                             batch_size = batch.size,
                             epochs = 100,
                             callbacks = callbacks,
                             shuffle = FALSE)
    
    return(model)
}  


# ------------------------------------------------------------------------------------
# 번  호: 11
# 함수명: rnn_fn
# 입력값: train.X(훈련용 데이터의 독립변수 값), train.Y(훈련용 데이터의 종속변수 값), 
#         batch.size(훈련용 데이터를 여러 작은 그룹으로 나누었을 때 하나의 소그룹에 속하는 데이터 수), 
#         units(layer 안의 node의 수), 
#         epsilon(학습 속도), pred(출력node의 수), times(선행(lead) 또는 지연(lag) 할 시점)
# 설  명: RNN모델 학습
# ------------------------------------------------------------------------------------

rnn_fn <- function(train.X, train.Y, batch.size = 40, units = 30, epsilon = 0.1, pred = 1, times = 1){
    callbacks <- list(callback_early_stopping(patience = 200, monitor = 'loss'))  
    model <- keras_model_sequential()  
    batch.size <- batch.size
  
    model %>% 
      layer_simple_rnn(input_shape = c(times, dim(train.X)[3]),
                       units = units,
                       activation = 'relu',
                       return_sequences = T) %>% 
      time_distributed(layer_dense(units = pred))
  
  
    model %>%
      compile(loss = 'mse',
              # optimizer = optimizer,
              optimizer = optimizer_adagrad(lr = epsilon), metrics = list('mean_squared_error'))
  
    history <- model %>% fit(x = train.X, y = train.Y,
                             batch_size = batch.size,
                             epochs = 100,
                             callbacks = callbacks,
                             shuffle = FALSE)
    return(model)
}  


# ------------------------------------------------------------------------------------
# 번  호: 12
# 함수명: lstm_fn
# 입력값: train.X(훈련용 데이터의 독립변수 값), train.Y(훈련용 데이터의 종속변수 값), 
#         batch.size(훈련용 데이터를 여러 작은 그룹으로 나누었을 때 하나의 소그룹에 속하는 데이터 수), 
#         units(layer 안의 node의 수), 
#         epsilon(학습 속도), pred(출력node의 수), times(선행(lead) 또는 지연(lag) 할 시점)
# 설  명: LSTM모델 학습
# ------------------------------------------------------------------------------------

lstm_fn <- function(train.X, train.Y, batch.size = 100, units = 20, epsilon = 0.007, pred = 1, times = 1){  
    callbacks <- list(callback_early_stopping(patience = 200, monitor = 'loss')) 
    # use_session_with_seed(174)
    model <- keras_model_sequential()  
    batch.size <- batch.size
  
    model %>% 
      layer_lstm( 
        # batch_input_shape  = c(batch.size, 5, 108),
        input_shape  = c(times, dim(train.X)[3]),
        units = units,
        # dropout = 0.4 ,
        # recurrent_dropout = 0.6,
        # activation = 'tanh',
        return_sequences = T) %>% 
      layer_activation_leaky_relu(
        #) %>% layer_batch_normalization(
        ) %>% time_distributed(layer_dense(units = pred))
  
  
    model %>%
      compile(
        loss = 'mse',
        # optimizer = optimizer,
        optimizer = optimizer_adagrad(lr = epsilon),
        #(
        metrics = list('mean_squared_error'))
  
    history <- model %>% 
                 fit(x = train.X,
                     y = train.Y,
                     # validation_data = list(X.test.3D, Y.test.3D),
                     batch_size = batch.size,
                     epochs = 100,
                     callbacks = callbacks,
                     shuffle = FALSE)
    return(model)
}

              