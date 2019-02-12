## ios推送服务

### 配置apns.config

- **erl4apns:**

    cert.pem是对应的推送证书
    
    前端提供 `.p12` 文件类型的证书文件
    
    在priv路径下执行 `./p12-pem.sh xxx.p12 xxx.pem`
    
    会生成 `xxx.pem` 的证书

- **deveice_token_server:**
    
    `http_server`是用来获取uid对应的device_token的http服务
    
- **emysql:**

    `manager_tool_sup`中屏蔽了emysql的启动

- **sasl:**

    日志文件路径
    
### 单独发送一段测试文字

```erlang
{ok, Apnsconn} = apns_pool:checkout(apns_pool),
erl4apns:send_message(Apnsconn, "DeviceToken", "SendMsg", Num, <<"default">>).
```

## erlang版本

```
erlang18.0
```

### 注意

由于担心deps中的依赖丢失,

所以deps目录也作为项目的一部分